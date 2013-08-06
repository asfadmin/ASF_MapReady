#include "asf_meta.h"
#include "asf_iso_meta.h"
#include "asf_nan.h"
#include <stdio.h>
#include <time.h>
#include "dateUtil.h"

static void utcDateTime(iso_dateTime *dt)
{
  time_t t = time(NULL);
  struct tm *gmt = gmtime(&t);
  dt->year = gmt->tm_year + 1900;
  dt->month = gmt->tm_mon + 1;
  dt->day = gmt->tm_mday;
  dt->hour = gmt->tm_hour;
  dt->min = gmt->tm_min;
  dt->second = (double) gmt->tm_sec;
}

static void dateTimeStamp(meta_parameters *meta, int line, 
			  iso_dateTime *dateTime)
{
  julian_date jd;
  hms_time hms;
  ymd_date ymd;
  jd.year = meta->state_vectors->year;
  jd.jd = meta->state_vectors->julDay;
  date_sec2hms(meta->state_vectors->second, &hms);
  date_jd2ymd(&jd, &ymd);
  double imgSec = line*meta->sar->azimuth_time_per_pixel;
  add_time(imgSec, &ymd, &hms);
  dateTime->year = ymd.year;
  dateTime->month = ymd.month;
  dateTime->day = ymd.day;
  dateTime->hour = hms.hour;
  dateTime->min = hms.min;
  dateTime->second = hms.sec;
}

static double rangeTime(meta_parameters *meta, int sample)
{
  return (meta_get_slant(meta, 0.0, (double) sample) / SPD_LIGHT);
}

static void dateTime2str(iso_dateTime timeUTC, char *str)
{
  char mon[][5]=
    {"","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};

  int year = timeUTC.year;
  int month = timeUTC.month;
  int day = timeUTC.day;
  int hour = timeUTC.hour;
  int min = timeUTC.min;
  double sec = timeUTC.second;

  sprintf(str, "%02d-%s-%4d %02d:%02d:%02.0f", 
	    day, mon[month], year, hour, min, sec);
}

iso_meta *meta2iso(meta_parameters *meta)
{
  int ii, kk, numAnnotations, numLayers, numAuxRasterFiles;
  iso_polLayer_t *polLayer;
  char **beamID, errorMessage[1024];
  int line_count = meta->general->line_count;
  int sample_count = meta->general->sample_count;

  strcpy(errorMessage, "");
  if (!meta->sar)
    strcat(errorMessage, "Missing SAR block. Can't generate ISO metadata\n");
  else if (!meta->state_vectors)
    strcat(errorMessage, 
	   "Missing state vector block. Cant't generate ISO metadata\n");
  if (strlen(errorMessage) > 0)
    asfPrintError(errorMessage);

  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    numAnnotations = 1;
    numLayers = 1;
    numAuxRasterFiles = 0;
    polLayer = (iso_polLayer_t *) CALLOC(1, sizeof(iso_polLayer_t));
    polLayer[0] = HH_POL;
    beamID = (char **) CALLOC(1, sizeof(char *));
    beamID[0] = (char *) CALLOC(20, sizeof(char));
    strcpy(beamID[0], meta->general->sensor_name);
  }

  iso_meta *iso = iso_meta_init();
  iso_generalHeader *header = iso->generalHeader;
  iso_productComponents *comps = iso->productComponents;
  iso_productInfo *info = iso->productInfo;
  iso_productSpecific *spec = iso->productSpecific;
  iso_setup *setup = iso->setup;
  iso_processing *proc = iso->processing;
  iso_instrument *inst = iso->instrument;
  iso_platform *platform = iso->platform;
  iso_productQuality *quality = iso->productQuality;

  // General Header  
  strcpy(header->itemName, "LEVEL 1 PRODUCT");
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)  
    strcpy(header->mission, "SEASAT");
  strcpy(header->source, "(seasat tape)");
  strcpy(header->destination, "DATAPOOL");
  strncpy(header->generationSystem, meta->general->processor, 20);
  utcDateTime(&header->generationTime);
  // header->referenceDocument: needs to be generated
  header->revision = (char *) CALLOC(20, sizeof(char));
  strcpy(header->revision, "OPERATIONAL");

  // Product Components
  comps->numAnnotations = numAnnotations;
  comps->numLayers = numLayers;
  comps->numAuxRasterFiles = numAuxRasterFiles;
  comps->annotation = 
    (iso_filesType *) CALLOC(numAnnotations, sizeof(iso_filesType));
  for (ii=0; ii<numAnnotations; ii++) {
    comps->annotation[ii].type = MAIN_TYPE;
    strcpy(comps->annotation[ii].file.host, ".");
    strcpy(comps->annotation[ii].file.path, ".");
    sprintf(comps->annotation[ii].file.name, "%s.xml", meta->general->basename);
    comps->annotation[ii].file.size = -1;
  }
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    // only one HDF5 file that contains everything
    comps->imageData = (iso_filesPol *) CALLOC(1,sizeof(iso_filesPol));
    comps->imageData[0].polLayer = HH_POL;
    strcpy(comps->imageData[0].beamID, meta->general->sensor_name);
    strcpy(comps->imageData[0].file.host, ".");
    strcpy(comps->imageData[0].file.path, ".");
    sprintf(comps->imageData[0].file.name, "%s.h5", meta->general->basename);
    comps->imageData[0].file.size = -1;
  }
  comps->quicklooks = 
    (iso_filesPol *) CALLOC(numLayers, sizeof(iso_filesPol));
  for (ii=0; ii<numLayers; ii++) {
    comps->quicklooks[ii].polLayer = polLayer[ii];
    strcpy(comps->quicklooks[ii].beamID, beamID[ii]);
    strcpy(comps->quicklooks[ii].file.host, ".");
    strcpy(comps->quicklooks[ii].file.path, ".");
    sprintf(comps->quicklooks[ii].file.name, "%s.jpg", 
	    meta->general->basename);
    // comps->quicklooks[ii].file.size: calculated after being generated
  }
  strcpy(comps->browseImage.host, ".");
  strcpy(comps->browseImage.path, ".");
  sprintf(comps->browseImage.name, "%s.jpg", meta->general->basename);
  // comps->browseImage.size: calculated after being generated
  strcpy(comps->mapPlot.host, ".");
  strcpy(comps->mapPlot.path, ".");
  sprintf(comps->mapPlot.name, "%s.kml", meta->general->basename);
  // comps->mapPlat.size: calculated after being generated

  // Product Info
  strcpy(info->logicalProductID, "not applicable");
  strcpy(info->receivingStation, meta->general->receiving_station);
  //strcpy(info->receivingStation, MAGIC_UNSET_STRING);
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    strcpy(info->level0ProcessingFacility, "ASF");
    strcpy(info->level1ProcessingFacility, "ASF");
  }
  info->groundOperationsType = OPERATIONAL;
  strcpy(info->deliveryInfo, "NOMINAL");
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)   
    strcpy(info->copyrightInfo, "Copyright NASA (1978)");
  // FIXME: need to decide whether quality inspection is constant or
  //        information comes from somewhere else
  info->qualityInspection = UNDEF_QUALITY; 
  strcpy(info->mission, meta->general->sensor);
  info->orbitPhase = 1; // nominal orbit
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    // FIXME: mid-Aug 1978 the orbit was changed to 244 revolution cycles
    info->numOrbitsInCycle = 43;
  }
  info->absOrbit = meta->general->orbit;
  info->orbitCycle = (info->absOrbit / info->numOrbitsInCycle) + 1;
  info->relOrbit = 
    info->absOrbit - (info->orbitCycle - 1)*info->numOrbitsInCycle; 
  if (meta->general->orbit_direction == 'A')
    info->orbitDirection = ASCENDING;
  else if (meta->general->orbit_direction == 'D')
    info->orbitDirection = DESCENDING;
  strncpy(info->sensor, meta->general->sensor_name, 20);
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)   
    info->imageMode = STANDARD_BEAM;
  if (meta->sar->look_direction == 'R')
    info->lookDirection = RIGHT_LOOK;
  else if (meta->sar->look_direction == 'L')
    info->lookDirection = LEFT_LOOK;
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    info->polarizationMode = SINGLE_POL;
    info->polLayer = (iso_polLayer_t *) CALLOC(1,sizeof(iso_polLayer_t));
    info->polLayer[0] = HH_POL;
    strcpy(info->elevationBeamConfiguration, meta->general->mode);
  }
  strcpy(info->azimuthBeamID, "boresightAzimuth");
  /* ScanSAR and spotlight
  info->numberOfBeams = MAGIC_UNSET_INT;
  info->beamID = NULL;
  info->numberOfBursts = MAGIC_UNSET_INT;
  info->numberOfAzimuthBeams = MAGIC_UNSET_INT;
  strcpy(info->azimuthBeamIDFirst, MAGIC_UNSET_STRING);
  strcpy(info->azimuthBeamIDLast, MAGIC_UNSET_STRING);
  info->azimuthSteeringAngleFirst = MAGIC_UNSET_DOUBLE;
  info->azimuthSteeringAngleLast = MAGIC_UNSET_DOUBLE;
  */
  // FIXME: work out naming scheme for productType
  strcpy(info->productType, "STANDARD PRODUCT");
  if (meta->general->data_type >= COMPLEX_BYTE)
    info->productVariant = SLC_PRODUCT;
  else
    info->productVariant = STD_PRODUCT;
  if (meta->sar->image_type == 'S')
    info->projection = SLANTRANGE_PROJ;
  else if (meta->sar->image_type == 'G');
    info->projection = GROUNDRANGE_PROJ;
  info->mapProjection = UNDEF_MAP;
  info->resolutionVariant = UNDEF_RES;
  info->radiometricCorrection = NOTCALIBRATED;
  // FIXME: needs to updated when calibration is done
  strcpy(info->pixelValueID, "RADAR BRIGHTNESS");
  if (meta->general->data_type >= COMPLEX_BYTE)
    info->imageDataType = COMPLEX_DATA_TYPE;
  else
    info->imageDataType = DETECTED_DATA_TYPE;
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)   
    info->imageDataFormat = HDF5_DATA_FORMAT;
  info->numberOfLayers = meta->general->band_count;
  if (meta->general->data_type == ASF_BYTE ||
      meta->general->data_type == COMPLEX_BYTE)
    info->imageDataDepth = 8;
  else if (meta->general->data_type == INTEGER16 ||
	   meta->general->data_type == COMPLEX_INTEGER16)
    info->imageDataDepth = 16;
  else if (meta->general->data_type == REAL32 ||
	   meta->general->data_type == INTEGER32 ||
	   meta->general->data_type == COMPLEX_REAL32 ||
	   meta->general->data_type == COMPLEX_INTEGER32)
    info->imageDataDepth = 32;
  else if (meta->general->data_type == REAL64 ||
	   meta->general->data_type == COMPLEX_REAL64)
    info->imageDataDepth = 64;
  info->imageStorageOrder = ROWBYROW;
  strcpy(info->rowContent, "RANGELINES");
  strcpy(info->columnContent, "AZIMUTHLINES");
  info->numberOfRows = meta->general->line_count;
  info->numberOfColumns = meta->general->sample_count;
  info->startRow = meta->general->start_line;
  info->startColumn = meta->general->start_sample;
  info->rowScaling = meta->general->line_scaling;
  info->columnScaling = meta->general->sample_scaling;
  info->rowSpacing = (float) meta->sar->azimuth_time_per_pixel;
  info->columnSpacing = (float) meta->sar->range_time_per_pixel;
  if (meta->sar->image_type == 'S') {
    spec->slantRangeResolution = meta->general->x_pixel_size;
    // FIXME: calculate groundRangeResolution for slant range
    info->groundRangeResolution = meta->general->x_pixel_size;
  }
  if (meta->sar->image_type == 'G') {
    // FIXME: calculate slantRangeResolution for ground range
    spec->slantRangeResolution = meta->general->x_pixel_size;
    info->groundRangeResolution = meta->general->x_pixel_size;
  }
  info->azimuthResolution = meta->general->y_pixel_size;
  info->azimuthLooks = (float) meta->sar->azimuth_look_count;
  info->rangeLooks = (float) meta->sar->range_look_count;
  strcpy(info->sceneID, meta->general->basename);
  if (meta->sar->azimuth_time_per_pixel < 0) {
    dateTimeStamp(meta, line_count, &info->startTimeUTC);
    dateTimeStamp(meta, 0, &info->stopTimeUTC);
  }
  else {
    dateTimeStamp(meta, 0, &info->startTimeUTC);
    dateTimeStamp(meta, line_count, &info->stopTimeUTC);
  }
  info->rangeTimeFirstPixel = rangeTime(meta, 0);
  info->rangeTimeLastPixel = rangeTime(meta, sample_count);
  info->sceneAzimuthExtent = line_count * meta->general->y_pixel_size;
  info->sceneRangeExtent = sample_count * meta->general->x_pixel_size;
  int *x = (int *) CALLOC(4, sizeof(int));
  int *y = (int *) CALLOC(4, sizeof(int));
  double lat, lon;
  y[0] = meta->general->line_count / 2;
  info->sceneCenterCoord.refRow = y[0];
  x[0] = meta->general->sample_count / 2;
  info->sceneCenterCoord.refColumn = x[0];
  if (meta->general->center_latitude == MAGIC_UNSET_DOUBLE ||
      meta->general->center_longitude == MAGIC_UNSET_DOUBLE) {
    info->sceneCenterCoord.lat = meta->general->center_latitude;
    info->sceneCenterCoord.lon = meta->general->center_longitude;
  }
  else {
    meta_get_latLon(meta, (double) y[0], (double) x[0], 0.0, &lat, &lon);
    info->sceneCenterCoord.lat = lat;
    info->sceneCenterCoord.lon = lon;    
  }
  dateTimeStamp(meta, y[0], &info->sceneCenterCoord.azimuthTimeUTC);
  info->sceneCenterCoord.rangeTime = rangeTime(meta, x[0]);
  if (ISNAN(meta->sar->incid_a[0]))
    info->sceneCenterCoord.incidenceAngle = meta_incid(meta, y[0], x[0])*R2D;
  else
    info->sceneCenterCoord.incidenceAngle = meta->sar->incid_a[0];
  info->sceneAverageHeight = MAGIC_UNSET_DOUBLE;
  x[0] = 0; y[0] = 0;
  x[1] = meta->general->sample_count; y[1] = 0;
  x[2] = 0; y[2] = meta->general->line_count;
  x[3] = meta->general->sample_count; y[3] = meta->general->line_count;
  for (ii=0; ii<4; ii++) {
    info->sceneCornerCoord[ii].refRow = y[ii];
    info->sceneCornerCoord[ii].refColumn = x[ii];
    meta_get_latLon(meta, (double) y[ii], (double) x[ii], 0.0, &lat, &lon);
    info->sceneCornerCoord[ii].lat = (float) lat;
    info->sceneCornerCoord[ii].lon = (float) lon;
    dateTimeStamp(meta, y[ii], &info->sceneCornerCoord[ii].azimuthTimeUTC);
    info->sceneCornerCoord[ii].rangeTime = rangeTime(meta, x[ii]);
    info->sceneCornerCoord[ii].incidenceAngle = 
      meta_incid(meta, (double) y[ii], (double) x[ii])*R2D;
  }
  info->yaw = meta->sar->yaw;
  info->pitch = meta->sar->pitch;
  info->roll = meta->sar->roll;
  info->earthRadius = meta->sar->earth_radius;
  info->satelliteHeight = meta->sar->satellite_height;
  info->headingAngle = meta->sar->heading_angle;
  strcpy(info->quicklooks.imageDataFormat,"JPEG");
  info->quicklooks.imageDataDepth = 8;
  info->quicklooks.numberOfRows = 1000;
  info->quicklooks.numberOfColumns = 1000;
  info->quicklooks.columnBlockLength = MAGIC_UNSET_DOUBLE;
  info->quicklooks.rowBlockLength = MAGIC_UNSET_DOUBLE;
  info->quicklooks.rowSpacing = MAGIC_UNSET_DOUBLE;
  info->quicklooks.columnSpacing = MAGIC_UNSET_DOUBLE;
  strcpy(info->browseImageDataFormat, "JPEG"); // assumption
  info->browseImageDataDepth = 8;
  strcpy(info->mapPlotFormat, "KML"); // assumption

  // Product Specific
  spec->commonPRF = meta->sar->prf;
  spec->commonRSF = meta->sar->range_sampling_rate;
  // FIXME: calculate properly for different geometry
  spec->slantRangeResolution = meta->general->x_pixel_size;
  spec->projectedSpacingAzimuth = meta->general->y_pixel_size;
  spec->projectedSpacingGroundNearRange = meta->general->x_pixel_size;
  spec->projectedSpacingGroundFarRange = meta->general->x_pixel_size;
  spec->projectedSpacingSlantRange = meta->sar->slant_range_first_pixel;
  spec->slantRangeShift = meta->sar->slant_shift;
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    spec->imageCoordinateType = RAW_COORD;
    spec->imageDataStartWith = EARLYAZNEARRG; // assumption
    spec->quicklookDataStartWith = EARLYAZNEARRG; // assumption
  }
  // FIXME: deal with map projected data later
  if (meta->projection) {
    spec->geocodedImageInfoFlag = TRUE;

    // mapProjection
    strcpy(spec->geodeticDatumID, MAGIC_UNSET_STRING);
    strcpy(spec->projectionID, MAGIC_UNSET_STRING);
    strcpy(spec->zoneID, MAGIC_UNSET_STRING);
    spec->projectionCenterLatitude = MAGIC_UNSET_DOUBLE;
    spec->projectionCenterLongitude = MAGIC_UNSET_DOUBLE;
    spec->mapOriginEasting = MAGIC_UNSET_DOUBLE;
    spec->mapOriginNorthing = MAGIC_UNSET_DOUBLE;
    spec->scaleFactor = MAGIC_UNSET_DOUBLE;
    
    // geoParameter
    spec->pixelSpacingEasting = MAGIC_UNSET_DOUBLE;
    spec->pixelSpacingNorthing = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsGeographic.upperLeftLatitude = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsGeographic.upperLeftLongitude = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsGeographic.upperRightLatitude = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsGeographic.upperRightLongitude = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsGeographic.lowerLeftLatitude = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsGeographic.lowerLeftLongitude = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsGeographic.lowerRightLatitude = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsGeographic.lowerRightLongitude = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsCartographic.upperLeftEasting = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsCartographic.upperLeftNorthing = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsCartographic.upperRightEasting = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsCartographic.upperRightNorthing = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsCartographic.lowerRightEasting = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsCartographic.lowerRightNorthing = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsCartographic.lowerLeftEasting = MAGIC_UNSET_DOUBLE;
    spec->frameCoordsCartographic.lowerLeftNorthing = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsGeographic.upperLeftLatitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsGeographic.upperLeftLongitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsGeographic.upperRightLatitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsGeographic.upperRightLongitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsGeographic.lowerLeftLatitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsGeographic.lowerLeftLongitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsGeographic.lowerRightLatitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsGeographic.lowerRightLongitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsCartographic.upperLeftEasting = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsCartographic.upperLeftNorthing = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsCartographic.upperRightEasting = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsCartographic.upperRightNorthing = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsCartographic.lowerRightEasting = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsCartographic.lowerRightNorthing = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsCartographic.lowerLeftEasting = MAGIC_UNSET_DOUBLE;
    spec->sceneCoordsCartographic.lowerLeftNorthing = MAGIC_UNSET_DOUBLE;
    spec->sceneCenterCoordLatitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCenterCoordLongitude = MAGIC_UNSET_DOUBLE;
    spec->sceneCenterCoordEasting = MAGIC_UNSET_DOUBLE;
    spec->sceneCenterCoordNorthing = MAGIC_UNSET_DOUBLE;
    spec->imageResamplingMethod = UNDEF_RESAMPLE;
    
    // elevationData
    spec->elevationDataFlag = FALSE;
    strcpy(spec->elevationDataSource, MAGIC_UNSET_STRING);
    spec->elevationMinimumHeight = MAGIC_UNSET_DOUBLE;
    spec->elevationMeanHeight = MAGIC_UNSET_DOUBLE;
    spec->elevationMaximumHeight = MAGIC_UNSET_DOUBLE;
    
    // incidenceAngleMaskDescription
    spec->incidenceAngleMaskDescriptionFlag = FALSE;
    strcpy(spec->incidenceAnglePixelValueID, MAGIC_UNSET_STRING);
    spec->incidenceAngleImageDataFormat = UNDEF_DATA_FORMAT;
    spec->incidenceAngleImageDataDepth = MAGIC_UNSET_INT;
    spec->incidenceAngleNumberOfRows = MAGIC_UNSET_INT;
    spec->incidenceAngleNumberOfColumns = MAGIC_UNSET_INT;
    spec->incidenceAngleRowSpacing = MAGIC_UNSET_DOUBLE;
    spec->incidenceAngleColumnSpacing = MAGIC_UNSET_DOUBLE;
  }

  // Setup
  strcpy(setup->orderType, "L1 STD");
  strcpy(setup->processingPriority, "NOMINAL");
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)
    setup->orbitAccuracy = RESTITUTED_ORBIT;
  setup->sceneSpecification = FRAME_SPEC;
  setup->frameID = meta->general->frame;
  dateTimeStamp(meta, 0, &setup->sceneStartTimeUTC);
  dateTimeStamp(meta, line_count, &setup->sceneStopTimeUTC);
  setup->sceneCenterLatitude = MAGIC_UNSET_DOUBLE;
  setup->sceneCenterLongitude = MAGIC_UNSET_DOUBLE;
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    setup->imagingMode = STANDARD_BEAM;
    setup->lookDirection = RIGHT_LOOK;
    setup->polarizationMode = SINGLE_POL;
    setup->polLayer = HH_POL;
    strcpy(setup->elevationBeamConfiguration, "STD");
  }
  if (meta->general->data_type >= COMPLEX_BYTE)  
    setup->productVariant = SLC_PRODUCT;
  else
    setup->productVariant = STD_PRODUCT;
  setup->resolutionVariant = UNDEF_RES;
  if (meta->sar->image_type == 'S')
    setup->projection = SLANTRANGE_PROJ;
  else if (meta->sar->image_type == 'G')
    setup->projection = GROUNDRANGE_PROJ;
  strcpy(setup->logicalDataTakeID, MAGIC_UNSET_STRING);
  strcpy(setup->level0ProductID, MAGIC_UNSET_STRING);
  setup->L0SARGenerationTimeUTC.year = MAGIC_UNSET_INT;
  setup->L0SARGenerationTimeUTC.month = MAGIC_UNSET_INT;
  setup->L0SARGenerationTimeUTC.day = MAGIC_UNSET_INT;
  setup->L0SARGenerationTimeUTC.hour = MAGIC_UNSET_INT;
  setup->L0SARGenerationTimeUTC.min = MAGIC_UNSET_INT;
  setup->L0SARGenerationTimeUTC.second = MAGIC_UNSET_DOUBLE;
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    setup->numProcessingSteps = 2;
    setup->processingStep = 
      (iso_procStep *) CALLOC(1,sizeof(iso_procStep)*setup->numProcessingSteps);
    strcpy(setup->processingStep[0].softwareID, "prep_raw");
    strcpy(setup->processingStep[0].softwareVersion, "1.2");
    strcpy(setup->processingStep[0].description, "pre-processing of raw data");
    strcpy(setup->processingStep[0].algorithm, 
	   "various level for cleaning up the header information");
    setup->processingStep[0].processingLevel = PRE_PROCESSING;
    setup->processingStep[0].processingTimeUTC.year = MAGIC_UNSET_INT;
    setup->processingStep[0].processingTimeUTC.month = MAGIC_UNSET_INT;
    setup->processingStep[0].processingTimeUTC.day = MAGIC_UNSET_INT;
    setup->processingStep[0].processingTimeUTC.hour = MAGIC_UNSET_INT;
    setup->processingStep[0].processingTimeUTC.min = MAGIC_UNSET_INT;
    setup->processingStep[0].processingTimeUTC.second = MAGIC_UNSET_DOUBLE;

    sprintf(setup->processingStep[1].softwareID, "%s", TOOL_SUITE_NAME);
    sprintf(setup->processingStep[1].softwareVersion, "%s", 
	    TOOL_SUITE_VERSION_STRING);
    strcpy(setup->processingStep[1].description, 
	   "processing of raw data to detected imagery");
    strcpy(setup->processingStep[1].algorithm, 
	   "customized ROI processing of raw data; "
	   "conversion of data from ROI to ASF format; "
	   "conversion of data from ASF to HDF5 format.");
    setup->processingStep[1].processingLevel = LEVEL_ONE;
    setup->processingStep[1].processingTimeUTC.year = MAGIC_UNSET_INT;
    setup->processingStep[1].processingTimeUTC.month = MAGIC_UNSET_INT;
    setup->processingStep[1].processingTimeUTC.day = MAGIC_UNSET_INT;
    setup->processingStep[1].processingTimeUTC.hour = MAGIC_UNSET_INT;
    setup->processingStep[1].processingTimeUTC.min = MAGIC_UNSET_INT;
    setup->processingStep[1].processingTimeUTC.second = MAGIC_UNSET_DOUBLE;
  }

  // Processing
  strcpy(proc->dopplerBasebandEstimationMethod, "azimuth cross correlation");
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)
    proc->dopplerCentroidCoordinateType = RAW_COORD;
  proc->doppler = (iso_dopplerCentroid *) CALLOC(1,sizeof(iso_dopplerCentroid));
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    proc->doppler[0].polLayer = HH_POL;
    proc->doppler[0].numberOfBlocks = MAGIC_UNSET_INT;
    proc->doppler[0].numberOfRejectedBlocks = MAGIC_UNSET_INT;
    proc->doppler[0].numberOfDopperRecords = 1;
    proc->doppler[0].timeUTC.year = MAGIC_UNSET_INT;
    proc->doppler[0].timeUTC.month = MAGIC_UNSET_INT;
    proc->doppler[0].timeUTC.day = MAGIC_UNSET_INT;
    proc->doppler[0].timeUTC.hour = MAGIC_UNSET_INT;
    proc->doppler[0].timeUTC.min = MAGIC_UNSET_INT;
    proc->doppler[0].timeUTC.second = MAGIC_UNSET_DOUBLE;
    proc->doppler[0].dopplerAtMidRange =
      meta_get_dop(meta, (double) line_count/2, (double) sample_count/2);
    proc->doppler[0].polynomialDegree = 2;
    proc->doppler[0].coefficient = (double *) CALLOC(3, sizeof(double));
    proc->doppler[0].coefficient[0] = meta->sar->range_doppler_coefficients[0];
    proc->doppler[0].coefficient[1] = meta->sar->range_doppler_coefficients[1];
    proc->doppler[0].coefficient[2] = meta->sar->range_doppler_coefficients[2];
  }
  // FIXME: ScanSAR will need processing parameters per beam
  proc->processingParameter = 
    (iso_processingParameter *) CALLOC(1, sizeof(iso_processingParameter));
  proc->processingParameter[0].processingInfoCoordinateType = RAW_COORD;
  proc->processingParameter[0].rangeLooks = (float) meta->sar->range_look_count;
  proc->processingParameter[0].azimuthLooks = 
    (float) meta->sar->azimuth_look_count;
  proc->processingParameter[0].rangeLookBandwidth = 0;
  proc->processingParameter[0].azimuthLookBandwidth =
    meta->sar->azimuth_processing_bandwidth;
  proc->processingParameter[0].totalProcessedRangeBandwidth = 0;
  proc->processingParameter[0].totalProcessedAzimuthBandwidth =
    meta->sar->azimuth_processing_bandwidth;
  proc->processingParameter[0].chirpRate = meta->sar->chirp_rate;
  proc->processingParameter[0].pulseDuration = meta->sar->pulse_duration;
  // rangeCompression ???
  // FIXME: check all the flags
  proc->chirpReplicaUsedFlag = TRUE;
  proc->geometricDopplerUsedFlag = FALSE;
  proc->azimuthPatternCorrectedFlag = FALSE;
  proc->elevationPatternCorrectedFlag = FALSE;
  if (meta->sar->image_type == 'S')
    proc->detectedFlag = FALSE;
  else if (meta->sar->image_type == 'G')
    proc->detectedFlag = TRUE;
  proc->multiLookedFlag = meta->sar->multilook;
  proc->polarimetricProcessedFlag = FALSE;
  proc->terrainCorrectedFlag = FALSE;
  proc->layoverShadowMaskGeneratedFlag = FALSE;
  proc->geocodedFlag = FALSE;
  proc->nominalProcessingPerformedFlag = TRUE;

  // Instrument
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)
    inst->instrumentInfoCoordinateType = RAW_COORD;
  inst->centerFrequency = SPD_LIGHT / meta->sar->wavelength;
  inst->settings = (iso_settings *) CALLOC(numLayers, sizeof(iso_settings));
  for (ii=0; ii<numLayers; ii++) {
    iso_settings set;
    if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
      set.polLayer = polLayer[ii];
      strcpy(set.beamID, beamID[ii]);
    }
    set.rxBandwidth = MAGIC_UNSET_DOUBLE;
    set.rsf = meta->sar->range_sampling_rate;
    set.numberOfPRFChanges = MAGIC_UNSET_INT;
    set.numberOfEchoWindowPositionChanges = MAGIC_UNSET_INT;
    set.numberOfEchoWindowLengthChanges = MAGIC_UNSET_INT;
    set.numberOfSettingRecords = MAGIC_UNSET_INT;
    int numRec = set.numberOfSettingRecords;
    numRec = 1;
    set.settingRecord = 
      (iso_settingRecord *) CALLOC(numRec, sizeof(iso_settingRecord));
    for (kk=0; kk<numRec; kk++) {
      iso_settingRecord rec;
      rec.startTimeUTC.year = MAGIC_UNSET_INT;
      rec.startTimeUTC.month = MAGIC_UNSET_INT;
      rec.startTimeUTC.day = MAGIC_UNSET_INT;
      rec.startTimeUTC.hour = MAGIC_UNSET_INT;
      rec.startTimeUTC.min = MAGIC_UNSET_INT;
      rec.startTimeUTC.second = MAGIC_UNSET_DOUBLE;
      rec.stopTimeUTC.year = MAGIC_UNSET_INT;
      rec.stopTimeUTC.month = MAGIC_UNSET_INT;
      rec.stopTimeUTC.day = MAGIC_UNSET_INT;
      rec.stopTimeUTC.hour = MAGIC_UNSET_INT;
      rec.stopTimeUTC.min = MAGIC_UNSET_INT;
      rec.stopTimeUTC.second = MAGIC_UNSET_DOUBLE;
      rec.numberOfRows = MAGIC_UNSET_INT;
      rec.prf = MAGIC_UNSET_DOUBLE;
      rec.echoWindowPosition = MAGIC_UNSET_DOUBLE;
      rec.echoWindowLength = MAGIC_UNSET_DOUBLE;
      strcpy(rec.pulseType, "standard");
      set.settingRecord[kk] = rec;
    }
    inst->settings[ii] = set;
  }

  // Platform
  platform->sensor = PREDICTED_SENSOR;
  platform->accuracy = RESTITUTED_ORBIT;
  strcpy(platform->stateVectorRefFrame, "WGS84");
  platform->stateVectorTimeSpacing = meta->state_vectors->vecs[1].time;
  platform->numStateVectors = meta->state_vectors->vector_count;
  platform->stateVec = 
    (iso_stateVec *) CALLOC(platform->numStateVectors, sizeof(iso_stateVec));
  hms_time hms;
  ymd_date ymd;
  if (meta->sar->azimuth_time_per_pixel > 0) {
    dateTimeStamp(meta, 0, &platform->firstStateTimeUTC); 
    dateTimeStamp(meta, meta->general->line_count, &platform->lastStateTimeUTC);
  }
  else {
    dateTimeStamp(meta, meta->general->line_count, &platform->firstStateTimeUTC); 
    dateTimeStamp(meta, 0, &platform->lastStateTimeUTC);
  }
  ymd.year = platform->firstStateTimeUTC.year;
  ymd.month = platform->firstStateTimeUTC.month;
  ymd.day = platform->firstStateTimeUTC.day;
  hms.hour = platform->firstStateTimeUTC.hour;
  hms.min = platform->firstStateTimeUTC.min;
  hms.sec = platform->firstStateTimeUTC.second;
  for (ii=0; ii<platform->numStateVectors; ii++) {
    if (ii > 0)
      add_time(platform->stateVectorTimeSpacing, &ymd, &hms);
    else
      add_time(meta->state_vectors->vecs[0].time, &ymd, &hms);
    platform->stateVec[ii].timeUTC.year = ymd.year;
    platform->stateVec[ii].timeUTC.month = ymd.month;
    platform->stateVec[ii].timeUTC.day = ymd.day;
    platform->stateVec[ii].timeUTC.hour = hms.hour;
    platform->stateVec[ii].timeUTC.min = hms.min;
    platform->stateVec[ii].timeUTC.second = hms.sec;
    platform->stateVec[ii].posX = meta->state_vectors->vecs[ii].vec.pos.x;
    platform->stateVec[ii].posY = meta->state_vectors->vecs[ii].vec.pos.y;
    platform->stateVec[ii].posZ = meta->state_vectors->vecs[ii].vec.pos.z;
    platform->stateVec[ii].velX = meta->state_vectors->vecs[ii].vec.vel.x;
    platform->stateVec[ii].velY = meta->state_vectors->vecs[ii].vec.vel.y;
    platform->stateVec[ii].velZ = meta->state_vectors->vecs[ii].vec.vel.z;
  }

  // Product Quality
  quality->rawDataQuality = 
    (iso_rawDataQuality *) CALLOC(numLayers, sizeof(iso_rawDataQuality));  
  for (ii=0; ii<numLayers; ii++) {
    quality->rawDataQuality[ii].polLayer = polLayer[ii];
    strcpy(quality->rawDataQuality[0].beamID, beamID[ii]);
    quality->rawDataQuality[ii].numGaps = 0;

    /* Information is now passed as gap file into iso_meta_write
    // need to get this information from somewhere else
    quality->rawDataQuality[ii].numGaps = 1;
    quality->rawDataQuality[ii].gap = (iso_gap *) CALLOC(1, sizeof(iso_gap));
    quality->rawDataQuality[ii].gap[0].start = 0;
    quality->rawDataQuality[ii].gap[0].length = MAGIC_UNSET_INT;
    quality->rawDataQuality[ii].gap[0].fill = RANDOM_FILL;
    quality->rawDataQuality[ii].gapSignificanceFlag = FALSE;
    quality->rawDataQuality[ii].missingLinesSignificanceFlag = FALSE;
    quality->rawDataQuality[ii].bitErrorSignificanceFlag = FALSE;
    quality->rawDataQuality[ii].timeReconstructionSignificanceFlag = FALSE;
    */
  }
  quality->dopplerAmbiguityNotZeroFlag = FALSE;
  quality->dopplerOutsideLimitsFlag = FALSE;
  quality->geolocationQualityLowFlag = FALSE;
  quality->imageDataQuality = 
    (iso_imageDataQuality *) CALLOC(numLayers, sizeof(iso_imageDataQuality));  
  for (ii=0; ii<numLayers; ii++) {
    quality->imageDataQuality[ii].polLayer = polLayer[ii];
    strcpy(quality->imageDataQuality[0].beamID, beamID[ii]);
    // need to get this information from somewhere else
    quality->imageDataQuality[ii].min = MAGIC_UNSET_DOUBLE;
    quality->imageDataQuality[ii].max = MAGIC_UNSET_DOUBLE;
    quality->imageDataQuality[ii].mean = MAGIC_UNSET_DOUBLE;
    quality->imageDataQuality[ii].stdDev = MAGIC_UNSET_DOUBLE;
    quality->imageDataQuality[ii].missingLines = meta->general->missing_lines;
    quality->imageDataQuality[ii].bitErrorRate = meta->general->bit_error_rate;
    quality->imageDataQuality[ii].noData = (double) meta->general->no_data;
  }
  quality->gapDefinition = 8;
  // These limits need to be defined per satellite
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    quality->gapPercentageLimit = MAGIC_UNSET_DOUBLE;
    quality->missingLinePercentageLimit = MAGIC_UNSET_DOUBLE;
    quality->bitErrorLimit = MAGIC_UNSET_DOUBLE;
    quality->timeReconstructionPercentageLimit = MAGIC_UNSET_DOUBLE;
    quality->dopplerCentroidLimit = MAGIC_UNSET_DOUBLE;
    quality->geolocationQualityLimit = MAGIC_UNSET_DOUBLE;
  }

  return iso;
}

static char *polLayer2str(iso_polLayer_t pol)
{
  char *str = (char *) CALLOC(20, sizeof(char));
  
  if (pol == HH_POL)
    strcpy(str, "HH");
  else if (pol == HV_POL)
    strcpy(str, "HV");
  else if (pol == VH_POL)
    strcpy(str, "VH");
  else if (pol == VV_POL)
    strcpy(str, "VV");
  else if (pol == UNDEF_POL_LAYER)
    strcpy(str, "UNDEFINED");

  return str;
}

meta_parameters *iso2meta(iso_meta *iso)
{
  int ii, kk;
  meta_parameters *meta = raw_init();
  char str[30];

  // Convenience pointers
  iso_generalHeader *header = iso->generalHeader;
  iso_productComponents *comps = iso->productComponents;
  iso_productInfo *info = iso->productInfo;
  iso_productSpecific *spec = iso->productSpecific;
  iso_setup *setup = iso->setup;
  iso_processing *proc = iso->processing;
  iso_instrument *inst = iso->instrument;
  iso_platform *platform = iso->platform;
  iso_productQuality *quality = iso->productQuality;

  meta->meta_version = 3.5;

  // General block
  for (ii=0; ii<comps->numAnnotations; ii++)
    if (comps->annotation[ii].type == MAIN_TYPE)
      strncpy(meta->general->basename, comps->annotation[ii].file.name, 256);
  strcpy(meta->general->sensor, header->mission);
  strcpy(meta->general->sensor_name, info->sensor);
  strcpy(meta->general->mode, info->elevationBeamConfiguration);
  strcpy(meta->general->receiving_station, info->receivingStation);
  strcpy(meta->general->processor, header->generationSystem);
  if (info->imageDataType == DETECTED_DATA_TYPE &&
      info->imageDataDepth == 8)
    meta->general->data_type = ASF_BYTE;
  else if (info->imageDataType == DETECTED_DATA_TYPE &&
	   info->imageDataDepth == 16)
    meta->general->data_type = INTEGER16;
  else if (info->imageDataType == DETECTED_DATA_TYPE &&
	   info->imageDataDepth == 32)
    // assumption here is that we are not dealing with INTERGER32
    meta->general->data_type = REAL32;
  else if (info->imageDataType == DETECTED_DATA_TYPE &&
	   info->imageDataDepth == 64)
    meta->general->data_type = REAL64;
  else if (info->imageDataType == COMPLEX_DATA_TYPE &&
	   info->imageDataDepth == 8)
    meta->general->data_type = COMPLEX_BYTE;
  else if (info->imageDataType == COMPLEX_DATA_TYPE &&
	   info->imageDataDepth == 16)
    meta->general->data_type = COMPLEX_INTEGER16;
  else if (info->imageDataType == COMPLEX_DATA_TYPE &&
	   info->imageDataDepth == 32)
    // assumption here is that we are not dealing with COMPLEX_INTEGER32
    meta->general->data_type = COMPLEX_REAL32;
  else if (info->imageDataType == COMPLEX_DATA_TYPE &&
	   info->imageDataDepth == 64)
    meta->general->data_type = COMPLEX_REAL64;
  if (info->imageDataType == RAW_DATA_TYPE)
    meta->general->image_data_type = RAW_IMAGE;
  else if (info->imageDataType == COMPLEX_DATA_TYPE)
    meta->general->image_data_type = COMPLEX_IMAGE;
  else if (info->imageDataType == DETECTED_DATA_TYPE)
    meta->general->image_data_type = AMPLITUDE_IMAGE;
  // more detailed mapping of imageDataType will probably need pixelValueID 
  // context
  if (strcmp_case(info->pixelValueID, "RADAR BRIGHTNESS") == 0)
    meta->general->radiometry = r_AMP;
  // dealing with any form of calibration not implemented yet
  dateTime2str(info->sceneCenterCoord.azimuthTimeUTC,
	       meta->general->acquisition_date);
  meta->general->orbit = info->absOrbit;
  if (info->orbitDirection == ASCENDING)  
    meta->general->orbit_direction = 'A';
  else if (info->orbitDirection == DESCENDING)
    meta->general->orbit_direction = 'D';
  meta->general->frame = setup->frameID;
  meta->general->band_count = comps->numLayers;
  strcpy(meta->general->bands, polLayer2str(comps->imageData[0].polLayer));
  for (ii=1; ii<comps->numLayers; ii++) {
    sprintf(str, ", %s", polLayer2str(comps->imageData[ii].polLayer));
    strcat(meta->general->bands, str);
  }
  int line_count = meta->general->line_count = info->numberOfRows;
  int sample_count = meta->general->sample_count = info->numberOfColumns;
  meta->general->start_line = info->startRow;
  meta->general->start_sample = info->startColumn;
  meta->general->x_pixel_size = info->groundRangeResolution;
  meta->general->y_pixel_size = info->azimuthResolution;
  meta->general->center_latitude = info->sceneCenterCoord.lat;
  meta->general->center_longitude = info->sceneCenterCoord.lon;
  spheroid_type_t spheroid = WGS84_SPHEROID; // FIXME: needs to know reference
  spheroid_axes_lengths(spheroid,
			&meta->general->re_major, &meta->general->re_minor);
  meta->general->bit_error_rate = quality->imageDataQuality[0].bitErrorRate;
  meta->general->missing_lines = quality->imageDataQuality[0].missingLines;
  meta->general->no_data = (float) quality->imageDataQuality[0].noData;

  // SAR block
  meta->sar = meta_sar_init();
  if (info->projection == SLANTRANGE_PROJ)
    meta->sar->image_type = 'S';
  else if (info->projection == GROUNDRANGE_PROJ)
    meta->sar->image_type = 'G';
  else if (info->projection == MAP_PROJ)
    meta->sar->image_type = 'P';
  if (setup->lookDirection == RIGHT_LOOK)
    meta->sar->look_direction = 'R';
  else if (setup->lookDirection == LEFT_LOOK)
    meta->sar->look_direction = 'L';
  meta->sar->azimuth_look_count = info->azimuthLooks;
  meta->sar->range_look_count = info->rangeLooks;
  if (spec->imageCoordinateType == RAW_COORD)
    meta->sar->deskewed = FALSE;
  else if (spec->imageCoordinateType == ZERODOPPLER)
    meta->sar->deskewed = TRUE;
  meta->general->line_scaling = info->rowScaling;  
  meta->general->sample_scaling = info->columnScaling;
  meta->sar->range_time_per_pixel = info->columnSpacing;
  meta->sar->azimuth_time_per_pixel = info->rowSpacing;
  meta->sar->slant_shift = spec->slantRangeShift;
  //meta->sar->slant_range_first_pixel = info->rangeTimeFirstPixel * SPD_LIGHT;
  meta->sar->slant_range_first_pixel = spec->projectedSpacingSlantRange;
  meta->sar->wavelength = SPD_LIGHT / inst->centerFrequency;
  meta->sar->prf = spec->commonPRF;
  meta->sar->earth_radius = info->earthRadius;
  meta->sar->satellite_height = info->satelliteHeight;
  // meta->sar->satellite_binary_time;
  // meta->sar->satellite_clock_time;
  for (ii=0; ii<=proc->doppler[0].polynomialDegree; ii++)
    meta->sar->range_doppler_coefficients[ii] = 
      proc->doppler[0].coefficient[ii];
  meta->sar->azimuth_doppler_coefficients[0] = proc->doppler[0].coefficient[0];
  // meta->sar->chirp_rate
  // meta->sar->pulse_duration
  meta->sar->range_sampling_rate = spec->commonRSF;
  if (info->polarizationMode == SINGLE_POL)
    strcpy(meta->sar->polarization, "SINGLE");
  else if (info->polarizationMode == DUAL_POL)
    strcpy(meta->sar->polarization, "DUAL");
  else if (info->polarizationMode == QUAD_POL)
    strcpy(meta->sar->polarization, "QUAD");
  meta->sar->multilook = proc->multiLookedFlag;
  meta->sar->pitch = info->pitch;
  meta->sar->roll = info->roll;
  meta->sar->yaw = info->yaw;
  meta->sar->incid_a[0] = info->sceneCenterCoord.incidenceAngle;
  meta->sar->heading_angle = info->headingAngle;
  meta->sar->chirp_rate = proc->processingParameter[0].chirpRate;
  meta->sar->pulse_duration = proc->processingParameter[0].pulseDuration;

  // meta->projection
  // meta->transform
  // meta->airsar
  // meta->uavsar
  // meta->statistics

  int numVectors = platform->numStateVectors;
  meta->state_vectors = meta_state_vectors_init(numVectors);
  meta->state_vectors->vector_count = numVectors;
  ymd_date ymdStart, ymdSV;
  julian_date jd;
  hms_time hmsStart, hmsSV;
  ymdStart.year = info->startTimeUTC.year;
  ymdStart.month = info->startTimeUTC.month;
  ymdStart.day = info->startTimeUTC.day;
  hmsStart.hour = info->startTimeUTC.hour;
  hmsStart.min = info->startTimeUTC.min;
  hmsStart.sec = info->startTimeUTC.second;
  meta->state_vectors->year = info->startTimeUTC.year;
  date_ymd2jd(&ymdStart, &jd);
  meta->state_vectors->julDay = jd.jd;
  meta->state_vectors->second = date_hms2sec(&hmsStart);
  for (ii=0; ii<numVectors; ii++) {
    ymdSV.year = platform->stateVec[ii].timeUTC.year;
    ymdSV.month = platform->stateVec[ii].timeUTC.month;
    ymdSV.day = platform->stateVec[ii].timeUTC.day;
    hmsSV.hour = platform->stateVec[ii].timeUTC.hour;
    hmsSV.min = platform->stateVec[ii].timeUTC.min;
    hmsSV.sec = platform->stateVec[ii].timeUTC.second;
    meta->state_vectors->vecs[ii].time = 
      time_difference(&ymdSV, &hmsSV, &ymdStart, &hmsStart);
    meta->state_vectors->vecs[ii].vec.pos.x = platform->stateVec[ii].posX;
    meta->state_vectors->vecs[ii].vec.pos.y = platform->stateVec[ii].posY;
    meta->state_vectors->vecs[ii].vec.pos.z = platform->stateVec[ii].posZ;
    meta->state_vectors->vecs[ii].vec.vel.x = platform->stateVec[ii].velX;
    meta->state_vectors->vecs[ii].vec.vel.y = platform->stateVec[ii].velY;
    meta->state_vectors->vecs[ii].vec.vel.z = platform->stateVec[ii].velZ;
  }
  if (meta->sar->azimuth_time_per_pixel > 0)
    meta->sar->time_shift = 0.0;
  else
    meta->sar->time_shift = meta->state_vectors->vecs[numVectors-1].time;
  /*
  // few calculations need state vectors
  meta->sar->earth_radius = 
    meta_get_earth_radius(meta, line_count/2, sample_count/2);
  // meta->sar->earth_radius_pp
  meta->sar->satellite_height =
    meta_get_sat_height(meta, meta->general->line_count/2,
			meta->general->sample_count/2);
  meta->sar->incid_a[0] = meta_incid(meta, line_count/2, sample_count/2)*R2D;
  */

  // Location block
  meta_get_corner_coords(meta);
/*
  meta->location = meta_location_init();
  for (ii=0; ii<4; ii++) {
    if (info->sceneCornerCoord[ii].refRow == 0 &&
	info->sceneCornerCoord[ii].refColumn == 0) {
      meta->location->lat_start_near_range = info->sceneCornerCoord[ii].lat;
      meta->location->lon_start_near_range = info->sceneCornerCoord[ii].lon;
    }
    else if (info->sceneCornerCoord[ii].refRow == 0 &&
	     info->sceneCornerCoord[ii].refColumn == sample_count) {
      meta->location->lat_start_far_range = info->sceneCornerCoord[ii].lat;
      meta->location->lon_start_far_range = info->sceneCornerCoord[ii].lon;
    }
    else if (info->sceneCornerCoord[ii].refRow == line_count &&
	     info->sceneCornerCoord[ii].refColumn == 0) {
      meta->location->lat_end_near_range = info->sceneCornerCoord[ii].lat;
      meta->location->lon_end_near_range = info->sceneCornerCoord[ii].lon;
    }
    else if (info->sceneCornerCoord[ii].refRow == line_count &&
	     info->sceneCornerCoord[ii].refColumn == sample_count) {
      meta->location->lat_end_far_range = info->sceneCornerCoord[ii].lat;
      meta->location->lon_end_far_range = info->sceneCornerCoord[ii].lon;
    }
  }
*/
  return meta;
}
