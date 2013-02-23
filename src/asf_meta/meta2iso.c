#include "asf_meta.h"
#include "asf_iso_meta.h"
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
    polLayer = (iso_polLayer_t *) MALLOC(sizeof(iso_polLayer_t));
    polLayer[0] = HH_POL;
    beamID = (char **) MALLOC(sizeof(char *));
    beamID[0] = (char *) MALLOC(sizeof(char)*20);
    strcpy(beamID[0], "STD");
  }

  iso_meta *iso = iso_meta_init();

  // General Header
  iso_generalHeader *header = iso->generalHeader;
  
  strcpy(header->itemName, "LEVEL 1 PRODUCT");
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)  
    strcpy(header->mission, "SEASAT");
  strcpy(header->source, "(seasat tape)");
  strcpy(header->destination, "DATAPOOL");
  strncpy(header->generationSystem, meta->general->processor, 20);
  utcDateTime(&header->generationTime);
  // header->referenceDocument: needs to be generated
  header->revision = (char *) MALLOC(sizeof(char)*20);
  strcpy(header->revision, "OPERATIONAL");

  // Product Components
  iso_productComponents *comps = iso->productComponents;

  comps->numAnnotations = numAnnotations;
  comps->numLayers = numLayers;
  comps->numAuxRasterFiles = numAuxRasterFiles;
  comps->annotation = 
    (iso_filesType *) MALLOC(sizeof(iso_filesType)*numAnnotations);
  for (ii=0; ii<numAnnotations; ii++) {
    comps->annotation[ii].type = MAIN_TYPE;
    strcpy(comps->annotation[ii].file.host, ".");
    strcpy(comps->annotation[ii].file.path, ".");
    strcpy(comps->annotation[ii].file.name, "(file basename).xml");
    comps->annotation[ii].file.size = -1;
  }
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    // only one HDF5 file that contains everything
    comps->imageData = (iso_filesPol *) MALLOC(sizeof(iso_filesPol));
    comps->imageData[0].polLayer = HH_POL;
    comps->imageData[0].beamID = (char *) MALLOC(sizeof(char)*20);
    strcpy(comps->imageData[0].beamID, "STD");
    strcpy(comps->imageData[0].file.host, ".");
    strcpy(comps->imageData[0].file.path, ".");
    strcpy(comps->imageData[0].file.name, "(file basename).h5");
    // comps->imageData[0].file.size: calculated after being generated
  }
  comps->quicklooks = 
    (iso_filesPol *) MALLOC(sizeof(iso_filesPol)*numLayers);
  for (ii=0; ii<numLayers; ii++) {
    comps->quicklooks[ii].polLayer = polLayer[ii];
    comps->quicklooks[ii].beamID = (char *) MALLOC(sizeof(char)*20);
    strcpy(comps->quicklooks[ii].beamID, beamID[ii]);
    strcpy(comps->quicklooks[ii].file.host, ".");
    strcpy(comps->quicklooks[ii].file.path, ".");
    strcpy(comps->quicklooks[ii].file.name, "(file basename)_(beam)_ql.tif");
    // comps->quicklooks[ii].file.size: calculated after being generated
  }
  strcpy(comps->browseImage.host, ".");
  strcpy(comps->browseImage.path, ".");
  strcpy(comps->browseImage.name, "(file basename)_browse.jpg");
  // comps->browseImage.size: calculated after being generated
  strcpy(comps->mapPlot.host, ".");
  strcpy(comps->mapPlot.path, ".");
  strcpy(comps->mapPlot.name, "(file basename)_plot.(ext)");
  // comps->mapPlat.size: calculated after being generated

  // Product Info
  iso_productInfo *info = (iso_productInfo *) iso->productInfo;

  strcpy(info->logicalProductID, "not applicable");
  //strcpy(info->receivingStation, meta->general->receiving_station);
  strcpy(info->receivingStation, MAGIC_UNSET_STRING);
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
  info->lookDirection = RIGHT_LOOK;
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0) {
    info->polarizationMode = SINGLE_POL;
    info->polLayer = (iso_polLayer_t *) MALLOC(sizeof(iso_polLayer_t));
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
  strcpy(info->productType, MAGIC_UNSET_STRING);
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
  strcpy(info->pixelValueID, "RADAR BRIGHTNESS");
  if (meta->general->data_type >= COMPLEX_BYTE)
    info->imageDataType = COMPLEX_DATA_TYPE;
  else
    info->imageDataType = DETECTED_DATA_TYPE;
  info->imageDataFormat = HDF5_DATA_FORMAT;
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)   
    info->numberOfLayers = 1;
  info->imageDataDepth = 32;
  info->imageStorageOrder = ROWBYROW;
  strcpy(info->rowContent, "RANGELINES");
  strcpy(info->columnContent, "AZIMUTHLINES");
  info->numberOfRows = meta->general->line_count;
  info->numberOfColumns = meta->general->sample_count;
  info->rowSpacing = (float) meta->sar->azimuth_time_per_pixel;
  info->columnSpacing = (float) meta->sar->range_time_per_pixel;
  // FIXME: calculate groundRangeResolution for slant range
  info->groundRangeResolution = MAGIC_UNSET_DOUBLE;
  info->azimuthResolution = meta->general->x_pixel_size;
  info->azimuthLooks = (float) meta->sar->azimuth_look_count;
  info->rangeLooks = (float) meta->sar->range_look_count;
  // FIXME: naming convention for sceneID
  strcpy(info->sceneID, MAGIC_UNSET_STRING);
  dateTimeStamp(meta, 0, &info->startTimeUTC);
  dateTimeStamp(meta, line_count, &info->stopTimeUTC);
  info->rangeTimeFirstPixel = rangeTime(meta, 0);
  info->rangeTimeLastPixel = rangeTime(meta, sample_count);
  // FIXME: calculate scene extents
  info->sceneAzimuthExtent = MAGIC_UNSET_DOUBLE;
  info->sceneRangeExtent = MAGIC_UNSET_DOUBLE;
  int *x = (int *) MALLOC(sizeof(int)*4);
  int *y = (int *) MALLOC(sizeof(int)*4);
  double lat, lon;
  y[0] = meta->general->line_count / 2;
  info->sceneCenterCoord.refRow = (int *) MALLOC(sizeof(int));
  *info->sceneCenterCoord.refRow = y[0];
  x[0] = meta->general->sample_count / 2;
  info->sceneCenterCoord.refColumn = (int *) MALLOC(sizeof(int));
  *info->sceneCenterCoord.refColumn = x[0];
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
  info->sceneCenterCoord.incidenceAngle = meta_incid(meta, y[0], x[0])*R2D;
  info->sceneAverageHeight = MAGIC_UNSET_DOUBLE;
  x[0] = 0; y[0] = 0;
  x[1] = meta->general->sample_count; y[1] = 0;
  x[2] = 0; y[2] = meta->general->line_count;
  x[3] = meta->general->sample_count; y[3] = meta->general->line_count;
  for (ii=0; ii<4; ii++) {
    info->sceneCornerCoord[ii].refRow = (int *) MALLOC(sizeof(int));
    *info->sceneCornerCoord[ii].refRow = y[ii];
    info->sceneCornerCoord[ii].refColumn = (int *) MALLOC(sizeof(int));
    *info->sceneCornerCoord[ii].refColumn = x[ii];
    meta_get_latLon(meta, (double) y[ii], (double) x[ii], 0.0, &lat, &lon);
    info->sceneCornerCoord[ii].lat = (float) lat;
    info->sceneCornerCoord[ii].lon = (float) lon;
    dateTimeStamp(meta, y[ii], &info->sceneCornerCoord[ii].azimuthTimeUTC);
    info->sceneCornerCoord[ii].rangeTime = rangeTime(meta, x[ii]);
    info->sceneCornerCoord[ii].incidenceAngle = 
      meta_incid(meta, (double) y[ii], (double) x[ii])*R2D;
  }
  info->headingAngle = meta->sar->heading_angle;
  strcpy(info->quicklooks.imageDataFormat,"TIFF");
  info->quicklooks.imageDataDepth = 16; // assumption
  info->quicklooks.numberOfRows = MAGIC_UNSET_INT;
  info->quicklooks.numberOfColumns = MAGIC_UNSET_INT;
  info->quicklooks.columnBlockLength = MAGIC_UNSET_DOUBLE;
  info->quicklooks.rowBlockLength = MAGIC_UNSET_DOUBLE;
  info->quicklooks.rowSpacing = MAGIC_UNSET_DOUBLE;
  info->quicklooks.columnSpacing = MAGIC_UNSET_DOUBLE;
  strcpy(info->browseImageDataFormat, "JPEG"); // assumption
  info->browseImageDataDepth = 8;
  strcpy(info->mapPlotFormat, "KML"); // assumption

  // Product Specific
  iso_productSpecific *spec = iso->productSpecific;

  spec->commonPRF = meta->sar->prf;
  spec->commonRSF = meta->sar->range_sampling_rate;
  spec->slantRangeResolution = MAGIC_UNSET_DOUBLE;
  spec->projectedSpacingAzimuth = MAGIC_UNSET_DOUBLE;
  spec->projectedSpacingGroundNearRange = MAGIC_UNSET_DOUBLE;
  spec->projectedSpacingGroundFarRange = MAGIC_UNSET_DOUBLE;
  spec->projectedSpacingSlantRange = MAGIC_UNSET_DOUBLE;
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
  iso_setup *setup = iso->setup;

  strcpy(setup->orderType, "???");
  strcpy(setup->processingPriority, "NOMINAL");
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)
    setup->orbitAccuracy = TLE;
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
    setup->numProcessingSteps = 1;
    setup->processingStep = (iso_procStep *) MALLOC(sizeof(iso_procStep));
    strcpy(setup->processingStep[0].softwareID, "prep_raw");
    strcpy(setup->processingStep[0].softwareVersion, "1.0");
    setup->processingStep[0].algorithm = (char *) MALLOC(sizeof(char)*255);
    strcpy(setup->processingStep[0].algorithm, 
	   "optional information about algorithm");
  }

  // Processing
  iso_processing *proc = iso->processing;

  strcpy(proc->dopplerBasebandEstimationMethod, "azimuth cross correlation");
  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)
    proc->dopplerCentroidCoordinateType = RAW_COORD;
  proc->doppler = (iso_dopplerCentroid *) MALLOC(sizeof(iso_dopplerCentroid));
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
    proc->doppler[0].coefficient = (double *) MALLOC(sizeof(double)*3);
    proc->doppler[0].coefficient[0] = meta->sar->range_doppler_coefficients[0];
    proc->doppler[0].coefficient[1] = meta->sar->range_doppler_coefficients[1];
    proc->doppler[0].coefficient[2] = meta->sar->range_doppler_coefficients[2];
  }
  // FIXME: ScanSAR will need processing parameters per beam
  proc->processingParameter = 
    (iso_processingParameter *) MALLOC(sizeof(iso_processingParameter));
  proc->processingParameter[0].processingInfoCoordinateType = RAW_COORD;
  proc->processingParameter[0].rangeLooks = meta->sar->range_look_count;
  proc->processingParameter[0].azimuthLooks = meta->sar->azimuth_look_count;
  proc->processingParameter[0].rangeLookBandwidth = 0;
  proc->processingParameter[0].azimuthLookBandwidth =
    meta->sar->azimuth_processing_bandwidth;
  proc->processingParameter[0].totalProcessedRangeBandwidth = 0;
  proc->processingParameter[0].totalProcessedAzimuthBandwidth =
    meta->sar->azimuth_processing_bandwidth;
  // rangeCompression ???
  // FIXME: check all the flags
  proc->chirpReplicaUsedFlag = TRUE;
  proc->geometricDopplerUsedFlag = FALSE;
  proc->azimuthPatternCorrectedFlag = FALSE;
  proc->elevationPatternCorrectedFlag = FALSE;
  if (meta->sar->image_type == 'S') {  
    proc->detectedFlag = FALSE;
    proc->multiLookedFlag = FALSE;
  }
  else if (meta->sar->image_type == 'G') {  
    proc->detectedFlag = TRUE;
    proc->multiLookedFlag = TRUE;
  }
  proc->polarimetricProcessedFlag = FALSE;
  proc->terrainCorrectedFlag = FALSE;
  proc->layoverShadowMaskGeneratedFlag = FALSE;
  proc->geocodedFlag = FALSE;
  proc->nominalProcessingPerformedFlag = TRUE;

  // Instrument
  iso_instrument *inst = iso->instrument;

  if (strcmp_case(meta->general->sensor, "SEASAT") == 0)
    inst->instrumentInfoCoordinateType = RAW_COORD;
  inst->centerFrequency = SPD_LIGHT / meta->sar->wavelength;
  inst->settings = (iso_settings *) MALLOC(sizeof(iso_settings)*numLayers);
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
      (iso_settingRecord *) MALLOC(sizeof(iso_settingRecord)*numRec);
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
  iso_platform *platform = iso->platform;

  platform->sensor = PREDICTED_SENSOR;
  platform->accuracy = TLE;
  strcpy(platform->stateVectorRefFrame, "WGS84");
  platform->stateVectorTimeSpacing = meta->state_vectors->vecs[1].time;
  platform->numStateVectors = meta->state_vectors->vector_count;
  platform->stateVec = 
    (iso_stateVec *) MALLOC(sizeof(iso_stateVec)*platform->numStateVectors);
  hms_time hms;
  ymd_date ymd;
  dateTimeStamp(meta, 0, &platform->firstStateTimeUTC); 
  dateTimeStamp(meta, meta->general->line_count, &platform->lastStateTimeUTC); 
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
  iso_productQuality *quality = iso->productQuality;

  quality->rawDataQuality = 
    (iso_rawDataQuality *) MALLOC(sizeof(iso_rawDataQuality)*numLayers);  
  for (ii=0; ii<numLayers; ii++) {
    quality->rawDataQuality[ii].polLayer = polLayer[ii];
    quality->rawDataQuality[0].beamID = (char *) MALLOC(sizeof(char)*20);
    strcpy(quality->rawDataQuality[0].beamID, beamID[ii]);
    // need to get this information from somewhere else
    quality->rawDataQuality[ii].numGaps = 1;
    quality->rawDataQuality[ii].gap = (iso_gap *) MALLOC(sizeof(iso_gap));
    quality->rawDataQuality[ii].gap[0].start = 0;
    quality->rawDataQuality[ii].gap[0].length = MAGIC_UNSET_INT;
    quality->rawDataQuality[ii].gap[0].fill = RANDOM_FILL;
    quality->rawDataQuality[ii].gapSignificanceFlag = FALSE;
    quality->rawDataQuality[ii].missingLinesSignificanceFlag = FALSE;
    quality->rawDataQuality[ii].bitErrorSignificanceFlag = FALSE;
    quality->rawDataQuality[ii].timeReconstructionSignificanceFlag = FALSE;
  }
  quality->dopplerAmbiguityNotZeroFlag = FALSE;
  quality->dopplerOutsideLimitsFlag = FALSE;
  quality->geolocationQualityLowFlag = FALSE;
  quality->imageDataQuality = 
    (iso_imageDataQuality *) MALLOC(sizeof(iso_imageDataQuality)*numLayers);  
  for (ii=0; ii<numLayers; ii++) {
    quality->imageDataQuality[ii].polLayer = polLayer[ii];
    quality->imageDataQuality[0].beamID = (char *) MALLOC(sizeof(char)*20);
    strcpy(quality->imageDataQuality[0].beamID, beamID[ii]);
    // need to get this information from somewhere else
    quality->imageDataQuality[ii].min = MAGIC_UNSET_DOUBLE;
    quality->imageDataQuality[ii].max = MAGIC_UNSET_DOUBLE;
    quality->imageDataQuality[ii].mean = MAGIC_UNSET_DOUBLE;
    quality->imageDataQuality[ii].stdDev = MAGIC_UNSET_DOUBLE;
  }
  quality->gapDefinition = MAGIC_UNSET_INT;
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
