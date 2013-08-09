#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xmlschemas.h>
#include "asf_iso_meta.h"
#include "xml_util.h"

static void str2dateTime(char *timeStr, iso_dateTime *dateTime)
{
  if (strcmp(timeStr, MAGIC_UNSET_STRING) == 0) {
    dateTime->year = MAGIC_UNSET_INT;
    dateTime->month = MAGIC_UNSET_INT;
    dateTime->day = MAGIC_UNSET_INT;
    dateTime->hour = MAGIC_UNSET_INT;
    dateTime->min = MAGIC_UNSET_INT;
    dateTime->second = MAGIC_UNSET_DOUBLE;
  }
  else 
    // UTC time stamp: 2008-03-13T22:19:55.140975
    sscanf(timeStr, "%d-%d-%dT%d:%d:%lfZ",
	   &dateTime->year, &dateTime->month, &dateTime->day, 
	   &dateTime->hour, &dateTime->min, &dateTime->second);
}

void validateXML(const char *xmlFile, const char *schemaFile)
{
  if (!fileExists(xmlFile))
    asfPrintError("Metadata file (%s) does not exist!\n", xmlFile);
  xmlDocPtr xmlDoc = xmlReadFile(xmlFile, NULL, 0);
  if (xmlDoc == NULL) {
    xmlFreeDoc(xmlDoc);
    asfPrintError("Could not parse metadata file (%s)\n", xmlFile);
  }
  xmlDocPtr schemaDoc = xmlReadFile(schemaFile, NULL, XML_PARSE_NONET);
  if (schemaDoc == NULL) {
    xmlFreeDoc(xmlDoc);
    xmlFreeDoc(schemaDoc);
    asfPrintError("Metadata file (%s) could not be validated\n"
		  "XML schema file (%s) cannot be loaded or is not "
		  "well-formed\n", xmlFile, schemaFile);
  }
  xmlSchemaParserCtxtPtr parserContext = xmlSchemaNewParserCtxt(schemaDoc);
  if (parserContext == NULL) {
    xmlFreeDoc(xmlDoc);
    xmlFreeDoc(schemaDoc);
    asfPrintError("Metadata file (%s) could not be validated\n"
		  "Unable to create a parser context for the XML schema (%s)\n",
		  xmlFile, schemaFile);
  }
  xmlSchemaPtr schema = xmlSchemaParse(parserContext);
  if (schema == NULL) {
    xmlFreeDoc(xmlDoc);
    xmlSchemaFreeParserCtxt(parserContext);
    xmlFreeDoc(schemaDoc);
    asfPrintError("Metadata file (%s) could not be validated\n"
		  "XML schema (%s) is not valid\n", xmlFile, schemaFile);
  }
  xmlSchemaValidCtxtPtr validContext = xmlSchemaNewValidCtxt(schema);
  if (validContext == NULL) {
    xmlFreeDoc(xmlDoc);
    xmlSchemaFree(schema);
    xmlSchemaFreeParserCtxt(parserContext);
    xmlFreeDoc(schemaDoc);
    asfPrintError("Metadata file (%s) could not be validated\n"
		  "Unable to create validation context for XML schema (%s)\n", 
		  xmlFile, schemaFile);
  }
  int isValid = (xmlSchemaValidateDoc(validContext, xmlDoc) == 0);
  xmlFreeDoc(xmlDoc);
  xmlSchemaFreeValidCtxt(validContext);
  xmlSchemaFree(schema);
  xmlSchemaFreeParserCtxt(parserContext);
  xmlFreeDoc(schemaDoc);
  if (!isValid)
    asfPrintError("Metadata file (%s) does not validate\n", xmlFile);
  else
    asfPrintStatus("Metadata file (%s) validates with schema (%s)\n",
		   xmlFile, schemaFile);
}

iso_meta *iso_meta_read(const char *xmlFile)
{
  int ii, kk, numAnnotations, numLayers, numAuxRasterFiles;
  iso_polLayer_t *polLayer;
  char **beamID, errorMessage[1024];
  char str[1024], element[1024];

  if (!fileExists(xmlFile))
    asfPrintError("Metadata file (%s) does not exist!\n", xmlFile);
  xmlDoc *doc = xmlReadFile(xmlFile, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file (%s)\n", xmlFile);
  // read schema file

  iso_meta *iso = iso_meta_init();
 
  // generalHeader
  iso_generalHeader *header = iso->generalHeader;
  
  strcpy(header->itemName,
	 xml_get_string_value(doc, "level1Product.generalHeader.itemName"));
  strcpy(header->mission, 
	 xml_get_string_value(doc, "level1Product.generalHeader.mission"));
  strcpy(header->source, 
	 xml_get_string_value(doc, "level1Product.generalHeader.source"));
  strcpy(header->destination, 
	 xml_get_string_value(doc, "level1Product.generalHeader.destination"));
  strcpy(header->generationSystem, xml_get_string_value(doc, 
    "level1Product.generalHeader.generationSystem"));
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.generalHeader.generationTime"));
  str2dateTime(str, &header->generationTime);
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.generalHeader.referenceDocument"));
  if (strcmp(str, MAGIC_UNSET_STRING) != 0) {
    header->referenceDocument = (char *) MALLOC(sizeof(char)*255);
    strcpy(header->referenceDocument, str);
  }
  strcpy(str, 
	 xml_get_string_value(doc, "level1Product.generalHeader.revision"));
  if (strcmp(str, MAGIC_UNSET_STRING) != 0) {
    header->revision = (char *) MALLOC(sizeof(char)*20);
    strcpy(header->revision, str);
  }
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.generalHeader.revisionComment"));
  if (strcmp(str, MAGIC_UNSET_STRING) != 0) {
    header->revisionComment = (char *) MALLOC(sizeof(char)*1024);
    strcpy(header->revisionComment, str);
  }

  // productComponents
  iso_productComponents *comps = iso->productComponents;

  if (strcmp_case(header->mission, "SEASAT") == 0) {
    numAnnotations = 1;
    numLayers = 1;
    numAuxRasterFiles = 0;
    polLayer = (iso_polLayer_t *) MALLOC(sizeof(iso_polLayer_t));
    polLayer[0] = HH_POL;
    beamID = (char **) MALLOC(sizeof(char *));
    beamID[0] = (char *) MALLOC(sizeof(char)*20);
    strcpy(beamID[0], "STD");
  }
  comps->annotation =
    (iso_filesType *) MALLOC(sizeof(iso_filesType)*numAnnotations);
  comps->numAnnotations = numAnnotations;
  comps->numLayers = numLayers;
  comps->numAuxRasterFiles = numAuxRasterFiles;
  for (ii=0; ii<comps->numAnnotations; ii++) {
    sprintf(element, "level1Product.productComponents.annotation[%d].type", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "MAIN") == 0)
      comps->annotation[ii].type = MAIN_TYPE;
    else if (strcmp_case(str, "GEOREF") == 0)
      comps->annotation[ii].type = GEOREF_TYPE;
    else if (strcmp_case(str, "GEOCODE") == 0)
      comps->annotation[ii].type = GEOCODE_TYPE;
    else if (strcmp_case(str, "OTHER") == 0)
      comps->annotation[ii].type = OTHER_TYPE;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      comps->annotation[ii].type = UNDEF_TYPE;
    sprintf(element, "level1Product.productComponents.annotation[%d].file.location.host", ii);
    strcpy(comps->annotation[ii].file.host, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.annotation[%d].file.location.path", ii);
    strcpy(comps->annotation[ii].file.path, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.annotation[%d].file.location.filename", ii);
    strcpy(comps->annotation[ii].file.name, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.annotation[%d].file.size", ii);
    comps->annotation[ii].file.size = xml_get_int_value(doc, element);
  }
  comps->imageData = (iso_filesPol *) MALLOC(sizeof(iso_filesPol)*numLayers);
  for (ii=0; ii<comps->numLayers; ii++) {
    sprintf(element, "level1Product.productComponents.imageData[%d].polLayer", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "HH") == 0)
      comps->imageData[ii].polLayer = HH_POL;
    else if (strcmp_case(str, "HV") == 0)
      comps->imageData[ii].polLayer = HV_POL;
    else if (strcmp_case(str, "VH") == 0)
      comps->imageData[ii].polLayer = VH_POL;
    else if (strcmp_case(str, "VV") == 0)
      comps->imageData[ii].polLayer = VV_POL;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      comps->imageData[ii].polLayer = UNDEF_POL_LAYER;
    sprintf(element, "level1Product.productComponents.imageData[%d].file.location.host", ii);
    strcpy(comps->imageData[ii].file.host, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.imageData[%d].file.location.path", ii);
    strcpy(comps->imageData[ii].file.path, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.imageData[%d].file.location.filename", ii);
    strcpy(comps->imageData[ii].file.name, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.imageData[%d].file.size", ii);
    comps->imageData[ii].file.size = xml_get_int_value(doc, element);
  }
  comps->quicklooks = (iso_filesPol *) MALLOC(sizeof(iso_filesPol)*numLayers);
  for (ii=0; ii<comps->numLayers; ii++) {
    sprintf(element, "level1Product.productComponents.quicklooks[%d].polLayer", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "HH") == 0)
      comps->quicklooks[ii].polLayer = HH_POL;
    else if (strcmp_case(str, "HV") == 0)
      comps->quicklooks[ii].polLayer = HV_POL;
    else if (strcmp_case(str, "VH") == 0)
      comps->quicklooks[ii].polLayer = VH_POL;
    else if (strcmp_case(str, "VV") == 0)
      comps->quicklooks[ii].polLayer = VV_POL;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      comps->quicklooks[ii].polLayer = UNDEF_POL_LAYER;
    sprintf(element, "level1Product.productComponents.quicklooks[%d].file.location.host", ii);
    strcpy(comps->quicklooks[ii].file.host, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.quicklooks[%d].file.location.path", ii);
    strcpy(comps->quicklooks[ii].file.path, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.quicklooks[%d].file.location.filename", ii);
    strcpy(comps->quicklooks[ii].file.name, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productComponents.quicklooks[%d].file.size", ii);
    comps->quicklooks[ii].file.size = xml_get_int_value(doc, element);
  }
  strcpy(comps->browseImage.host, xml_get_string_value(doc, 
    "level1Product.productComponents.browseImage.file.location.host"));
  strcpy(comps->browseImage.path, xml_get_string_value(doc,
    "level1Product.productComponents.browseImage.file.location.path"));
  strcpy(comps->browseImage.name, xml_get_string_value(doc,
    "level1Product.productComponents.browseImage.file.location.filename"));
  comps->browseImage.size = xml_get_int_value(doc,
    "level1Product.productComponents.browseImage.file.size");
  strcpy(comps->mapPlot.host, xml_get_string_value(doc,
    "level1Product.productComponents.mapPlot.file.location.host"));
  strcpy(comps->mapPlot.path, xml_get_string_value(doc,
    "level1Product.productComponents.mapPlot.file.location.path"));
  strcpy(comps->mapPlot.name, xml_get_string_value(doc,
    "level1Product.productComponents.mapPlot.file.location.filename"));
  comps->mapPlot.size = xml_get_int_value(doc,
    "level1Product.productComponents.mapPlot.file.size");

  // productInfo
  iso_productInfo *info = iso->productInfo;

  strcpy(info->logicalProductID, xml_get_string_value(doc,
    "level1Product.productInfo.generationInfo.logicalProductID"));
  strcpy(info->receivingStation, xml_get_string_value(doc,
    "level1Product.productInfo.generationInfo.receivingStation"));
  strcpy(info->level0ProcessingFacility, xml_get_string_value(doc,
    "level1Product.productInfo.generationInfo.level0ProcessingFacility"));
  strcpy(info->level1ProcessingFacility, xml_get_string_value(doc,
    "level1Product.productInfo.generationInfo.level1ProcessingFacility"));
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.generationInfo.groundOperationsType"));
  if (strcmp_case(str, "OPERATIONAL") == 0)
    info->groundOperationsType = OPERATIONAL;
  else if (strcmp_case(str, "PRE-OPERATIONAL") == 0)
    info->groundOperationsType = PREOPERATIONAL;
  else if (strcmp_case(str, "INSTRUMENT") == 0)
    info->groundOperationsType = INSTRUMENT;
  else if (strcmp_case(str, "TEST") == 0)
    info->groundOperationsType = TEST_OPS;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->groundOperationsType = UNDEF_OPS;
  strcpy(info->deliveryInfo, xml_get_string_value(doc,
    "level1Product.productInfo.generationInfo.deliveryInfo"));
  strcpy(info->copyrightInfo, xml_get_string_value(doc,
    "level1Product.productInfo.generationInfo.copyrightInfo"));
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.generationInfo.qualityInfo.qualityInspection"));
  if (strcmp_case(str, "AUTO APPROVED") == 0)
    info->qualityInspection = AUTO_APPROVED;
  else if (strcmp_case(str, "OPERATOR APPROVED") == 0)
    info->qualityInspection = OPERATOR_APPROVED;
  else if (strcmp_case(str, "NOT APPROVED") == 0)
    info->qualityInspection = NOT_APPROVED;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->qualityInspection = UNDEF_QUALITY;

  strcpy(info->mission, xml_get_string_value(doc,
    "level1Product.productInfo.missionInfo.mission"));
  info->orbitPhase = xml_get_int_value(doc,
    "level1Product.productInfo.missionInfo.orbitPhase");
  info->orbitCycle = xml_get_int_value(doc,
    "level1Product.productInfo.missionInfo.orbitCycle");
  info->absOrbit = xml_get_int_value(doc,
    "level1Product.productInfo.missionInfo.absOrbit");
  info->relOrbit = xml_get_int_value(doc,
    "level1Product.productInfo.missionInfo.relOrbit");
  info->numOrbitsInCycle = xml_get_int_value(doc,
    "level1Product.productInfo.missionInfo.numOrbitsInCycle");
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.missionInfo.orbitDirection"));
  if (strcmp_case(str, "ASCENDING") == 0)
    info->orbitDirection = ASCENDING;
  else if (strcmp_case(str, "DESCENDING") == 0)
    info->orbitDirection = DESCENDING;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->orbitDirection = UNDEF_ORBIT;

  strcpy(info->sensor, xml_get_string_value(doc,
    "level1Product.productInfo.acquisitionInfo.sensor"));
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.acquisitionInfo.imagingMode"));
  if (strcmp_case(str, "FINE BEAM") == 0)
    info->imageMode = FINE_BEAM;
  else if (strcmp_case(str, "STANDARD BEAM") == 0)
    info->imageMode = STANDARD_BEAM;
  else if (strcmp_case(str, "STRIPMAP") == 0)
    info->imageMode = STRIPMAP_IMAGE;
  else if (strcmp_case(str, "SCANSAR") == 0)
    info->imageMode = SCANSAR_IMAGE;
  else if (strcmp_case(str, "SPOTLIGHT") == 0)
    info->imageMode = SPOTLIGHT_IMAGE;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->imageMode = UNDEF_IMAGE_MODE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.acquisitionInfo.lookDirection"));
  if (strcmp_case(str, "RIGHT") == 0)
    info->lookDirection = RIGHT_LOOK;
  else if (strcmp_case(str, "LEFT") == 0)
    info->lookDirection = LEFT_LOOK;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->lookDirection = UNDEF_LOOK;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.acquisitionInfo.polarizationMode"));
  if (strcmp_case(str, "SINGLE") == 0)
    info->polarizationMode = SINGLE_POL;
  else if (strcmp_case(str, "DUAL") == 0)
    info->polarizationMode = DUAL_POL;
  else if (strcmp_case(str, "QUAD") == 0)
    info->polarizationMode = QUAD_POL;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->polarizationMode = UNDEF_POL_MODE;
  info->polLayer = (iso_polLayer_t *) MALLOC(sizeof(iso_polLayer_t)*numLayers);
  for (ii=0; ii<numLayers; ii++) {
    sprintf(element, "level1Product.productComponents.quicklooks[%d].polLayer", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "HH") == 0)
      info->polLayer[ii] = HH_POL;
    else if (strcmp_case(str, "HV") == 0)
      info->polLayer[ii] = HV_POL;
    else if (strcmp_case(str, "VH") == 0)
      info->polLayer[ii] = VH_POL;
    else if (strcmp_case(str, "VV") == 0)
      info->polLayer[ii] = VV_POL;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      info->polLayer[ii] = UNDEF_POL_LAYER;
  }
  strcpy(info->elevationBeamConfiguration, xml_get_string_value(doc,
    "level1Product.productInfo.acquisitionInfo.elevationBeamConfiguration"));
  strcpy(info->azimuthBeamID, xml_get_string_value(doc,
    "level1Product.productInfo.acquisitionInfo.imagingModeSpecificInfo.standardBeam.azimuthBeamID"));
  /* ScanSAR / Spotlight
  info->numberOfBeams = MAGIC_UNSET_INT;
  info->numberOfBursts = MAGIC_UNSET_INT;
  info->numberOfAzimuthBeams = MAGIC_UNSET_INT;
  strcpy(info->azimuthBeamIDFirst, MAGIC_UNSET_STRING);
  strcpy(info->azimuthBeamIDLast, MAGIC_UNSET_STRING);
  info->azimuthSteeringAngleFirst = MAGIC_UNSET_DOUBLE;
  info->azimuthSteeringAngleLast = MAGIC_UNSET_DOUBLE;
  */

  // productVariantInfo
  strcpy(info->productType, xml_get_string_value(doc,
    "level1Product.productInfo.productVariantInfo.productType"));
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.productVariantInfo.productVariant"));
  if (strcmp_case(str, "SLC") == 0)
    info->productVariant = SLC_PRODUCT;
  else if (strcmp_case(str, "STD") == 0)
    info->productVariant = STD_PRODUCT;
  else if (strcmp_case(str, "TC") == 0)
    info->productVariant = TC_PRODUCT;
  else if (strcmp_case(str, "RTC") == 0)
    info->productVariant = RTC_PRODUCT;
  else if (strcmp_case(str, "GEO") == 0)
    info->productVariant = GEO_PRODUCT;
  else if (strcmp_case(str, "SSC") == 0)
    info->productVariant = SSC_PRODUCT;
  else if (strcmp_case(str, "MGD") == 0)
    info->productVariant = MGD_PRODUCT;
  else if (strcmp_case(str, "GEC") == 0)
    info->productVariant = GEC_PRODUCT;
  else if (strcmp_case(str, "EEC") == 0)
    info->productVariant = EEC_PRODUCT;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->productVariant = UNDEF_PRODUCT;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.productVariantInfo.projection"));
  if (strcmp_case(str, "SLANTRANGE") == 0)
    info->projection = SLANTRANGE_PROJ;
  else if (strcmp_case(str, "GROUNDRANGE") == 0)
    info->projection = GROUNDRANGE_PROJ;
  else if (strcmp_case(str, "MAP") == 0)
    info->projection = MAP_PROJ;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->projection = UNDEF_PROJ;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.productVariantInfo.mapProjection"));
  if (strcmp_case(str, "UTM") == 0)
    info->mapProjection = UTM_PROJ;
  else if (strcmp_case(str, "POLARSTEREOGRAPHIC") == 0)
    info->mapProjection = PS_PROJ;
  else if (strcmp_case(str, "GEOGRAPHIC") == 0)
    info->mapProjection = GEOG_PROJ;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->mapProjection = UNDEF_MAP;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.productVariantInfo.resolutionVariant"));
  if (strcmp_case(str, "SE") == 0)
    info->resolutionVariant = SE_RES;
  else if (strcmp_case(str, "RE") == 0)
    info->resolutionVariant = RE_RES;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->resolutionVariant = UNDEF_RES;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.productVariantInfo.radiometricCorrection"));
  if (strcmp_case(str, "CALIBRATED") == 0)
    info->radiometricCorrection = CALIBRATED;
  else if (strcmp_case(str, "RELATIVE CALIBRATED") == 0)
    info->radiometricCorrection = RELCALIBRATED;
  else if (strcmp_case(str, "NOT CALIBRATED") == 0)
    info->radiometricCorrection = NOTCALIBRATED;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->radiometricCorrection = UNDEF_CAL;

  // imageDataInfo
  strcpy(info->pixelValueID, xml_get_string_value(doc,
    "level1Product.productInfo.imageDataInfo.pixelValueID"));
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.imageDataInfo.imageDataType"));
  if (strcmp_case(str, "DETECTED") == 0)
    info->imageDataType = DETECTED_DATA_TYPE;
  else if (strcmp_case(str, "COMPLEX") == 0)
    info->imageDataType = COMPLEX_DATA_TYPE;
  else if (strcmp_case(str, "RAW") == 0)
    info->imageDataType = RAW_DATA_TYPE;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->imageDataType = UNDEF_DATA_TYPE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.imageDataInfo.imageDataFormat"));
  if (strcmp_case(str, "CEOS") == 0)
    info->imageDataFormat = CEOS_DATA_FORMAT;
  else if (strcmp_case(str, "GEOTIFF") == 0)
    info->imageDataFormat = GEOTIFF_DATA_FORMAT;
  else if (strncmp_case(str, "HDF5", 4) == 0)
    info->imageDataFormat = HDF5_DATA_FORMAT;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->imageDataFormat = UNDEF_DATA_FORMAT;
  info->numberOfLayers = xml_get_int_value(doc,
    "level1Product.productInfo.imageDataInfo.numberOfLayers");
  info->imageDataDepth = xml_get_int_value(doc,
    "level1Product.productInfo.imageDataInfo.imageDataDepth");
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.imageDataInfo.imageStoreOrder"));
  if (strcmp_case(str, "ROWBYROW") == 0)
    info->imageStorageOrder = ROWBYROW;
  else if (strcmp_case(str, "COLBYCOL") == 0)
    info->imageStorageOrder = COLBYCOL;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    info->imageStorageOrder = UNDEF_STORE;
  strcpy(info->rowContent, xml_get_string_value(doc,
    "level1Product.productInfo.imageDataInfo.rowContent"));
  strcpy(info->columnContent, xml_get_string_value(doc,
    "level1Product.productInfo.imageDataInfo.columnContent"));
  info->numberOfRows = xml_get_int_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.numberOfRows");
  info->numberOfColumns = xml_get_int_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.numberOfColumns");
  info->startRow = xml_get_int_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.startRow");
  info->startColumn = xml_get_int_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.startColumn");
  info->rowScaling = xml_get_double_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.rowScaling");
  info->columnScaling = xml_get_double_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.columnScaling");
  info->rowSpacing = xml_get_double_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.rowSpacing");
  info->columnSpacing = xml_get_double_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.columnSpacing");
  info->groundRangeResolution = xml_get_double_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.groundRangeResolution");
  info->azimuthResolution = xml_get_double_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.azimuthResolution");
  info->azimuthLooks = xml_get_double_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.azimuthLooks");
  info->rangeLooks = xml_get_double_value(doc,
    "level1Product.productInfo.imageDataInfo.imageRaster.rangeLooks");

  strcpy(info->sceneID, xml_get_string_value(doc,
    "level1Product.productInfo.sceneInfo.sceneID"));
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.sceneInfo.start.timeUTC"));
  str2dateTime(str, &info->startTimeUTC);
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.sceneInfo.stop.timeUTC"));
  str2dateTime(str, &info->stopTimeUTC);
  info->rangeTimeFirstPixel = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.rangeTime.firstPixel");
  info->rangeTimeLastPixel = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.rangeTime.lastPixel");
  info->sceneAzimuthExtent = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.sceneAzimuthExtent");
  info->sceneRangeExtent = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.sceneRangeExtent");
  info->sceneCenterCoord.refRow = xml_get_int_value(doc,
    "level1Product.productInfo.sceneInfo.sceneCenterCoord.refRow");
  info->sceneCenterCoord.refColumn = xml_get_int_value(doc,
    "level1Product.productInfo.sceneInfo.sceneCenterCoord.refColumn");
  info->sceneCenterCoord.lat = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.sceneCenterCoord.lat");
  info->sceneCenterCoord.lon = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.sceneCenterCoord.lon");
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productInfo.sceneInfo.sceneCenterCoord.azimuthTimeUTC"));
  str2dateTime(str, &info->sceneCenterCoord.azimuthTimeUTC);
  info->sceneCenterCoord.rangeTime = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.sceneCenterCoord.rangeTime");
  info->sceneCenterCoord.incidenceAngle = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.sceneCenterCoord.incidenceAngle");
  info->sceneAverageHeight = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.sceneAverageHeight");
  for (ii=0; ii<4; ii++) {
    sprintf(str, "level1Product.productInfo.sceneInfo.sceneCornerCoord[%d].refRow", ii);
    info->sceneCornerCoord[ii].refRow = xml_get_int_value(doc, str);
    sprintf(str, "level1Product.productInfo.sceneInfo.sceneCornerCoord[%d].refColumn", ii);
    info->sceneCornerCoord[ii].refColumn = xml_get_int_value(doc, str);
    sprintf(str, "level1Product.productInfo.sceneInfo.sceneCornerCoord[%d].lat", ii);
    info->sceneCornerCoord[ii].lat = xml_get_double_value(doc, str);
    sprintf(str, "level1Product.productInfo.sceneInfo.sceneCornerCoord[%d].lon", ii);
    info->sceneCornerCoord[ii].lon = xml_get_double_value(doc, str);
    sprintf(element, "level1Product.productInfo.sceneInfo.sceneCornerCoord[%d].azimuthTimeUTC", ii);
    strcpy(str, xml_get_string_value(doc, element));
    str2dateTime(str, &info->sceneCornerCoord[ii].azimuthTimeUTC);
    sprintf(str, "level1Product.productInfo.sceneInfo.sceneCornerCoord[%d].rangeTime", ii);
    info->sceneCornerCoord[ii].rangeTime = xml_get_double_value(doc, str);
    sprintf(str, "level1Product.productInfo.sceneInfo.sceneCornerCoord[%d].incidenceAngle", ii);
    info->sceneCornerCoord[ii].incidenceAngle = xml_get_double_value(doc, str);
  }
  info->yaw = xml_get_double_value(doc, 
    "level1Product.productInfo.sceneInfo.yawAngle");
  info->pitch = xml_get_double_value(doc, 
    "level1Product.productInfo.sceneInfo.pitchAngle");
  info->roll = xml_get_double_value(doc, 
    "level1Product.productInfo.sceneInfo.rollAngle");
  info->earthRadius = xml_get_double_value(doc, 
    "level1Product.productInfo.sceneInfo.earthRadius");
  info->satelliteHeight = xml_get_double_value(doc, 
    "level1Product.productInfo.sceneInfo.satelliteHeight");
  info->headingAngle = xml_get_double_value(doc,
    "level1Product.productInfo.sceneInfo.headingAngle");

  // previewInfo
  strcpy(info->quicklooks.imageDataFormat, xml_get_string_value(doc,
    "level1Product.productInfo.previewInfo.quicklooks.imageDataFormat"));
  info->quicklooks.imageDataDepth = xml_get_int_value(doc,
    "level1Product.productInfo.previewInfo.quicklooks.imageDataDepth");
  info->quicklooks.numberOfRows = xml_get_int_value(doc,
    "level1Product.productInfo.previewInfo.quicklooks.imageRaster.numberOfRows");
  info->quicklooks.numberOfColumns = xml_get_int_value(doc,
    "level1Product.productInfo.previewInfo.quicklooks.imageRaster.numberOfColumns");
  info->quicklooks.columnBlockLength = xml_get_double_value(doc,
    "level1Product.productInfo.previewInfo.quicklooks.imageRaster.columnBlockLength");
  info->quicklooks.rowBlockLength = xml_get_double_value(doc,
    "level1Product.productInfo.previewInfo.quicklooks.imageRaster.rowBlockLength");
  info->quicklooks.rowSpacing = xml_get_double_value(doc,
    "level1Product.productInfo.previewInfo.quicklooks.imageRaster.rowSpacing");
  info->quicklooks.columnSpacing = xml_get_double_value(doc,
    "level1Product.productInfo.previewInfo.quicklooks.imageRaster.columnSpacing");
  /*
  strcpy(info->compositeQLImageDataFormat, MAGIC_UNSET_STRING);
  info->compositeQLImageDataDepth = MAGIC_UNSET_INT;
  info->compositeQLPolLayerCode = NULL;
  */
  strcpy(info->browseImageDataFormat, xml_get_string_value(doc,
    "level1Product.productInfo.previewInfo.browseImage.imageDataFormat"));
  info->browseImageDataDepth = xml_get_int_value(doc,
    "level1Product.productInfo.previewInfo.browseImage.imageDataDepth");
  strcpy(info->mapPlotFormat, xml_get_string_value(doc,
    "level1Product.productInfo.previewInfo.mapPlotFormat"));

  // productSpecific
  iso_productSpecific *spec = iso->productSpecific;

  spec->commonPRF = xml_get_double_value(doc,
    "level1Product.productSpecific.complexImageInfo.commonPRF");
  spec->commonRSF = xml_get_double_value(doc,
    "level1Product.productSpecific.complexImageInfo.commonRSF");
  spec->slantRangeResolution = xml_get_double_value(doc,
    "level1Product.productSpecific.complexImageInfo.slantRangeResolution");
  spec->projectedSpacingAzimuth = xml_get_double_value(doc,
    "level1Product.productSpecific.complexImageInfo.projectedSpacingAzimuth");
  spec->projectedSpacingGroundNearRange = xml_get_double_value(doc,
    "level1Product.productSpecific.complexImageInfo.projectedSpacingRange.groundNear");
  spec->projectedSpacingGroundFarRange = xml_get_double_value(doc,
    "level1Product.productSpecific.complexImageInfo.projectedSpacingRange.groundFar");
  spec->projectedSpacingSlantRange = xml_get_double_value(doc,
    "level1Product.productSpecific.complexImageInfo.projectedSpacingRange.slantRange");
  spec->slantRangeShift = xml_get_double_value(doc,
    "level1Product.productSpecific.complexImageInfo.projectedSpacingRange.slantRangeShift");
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productSpecific.complexImageInfo.imageCoordinateType"));
  if (strcmp_case(str, "RAW") == 0)
    spec->imageCoordinateType = RAW_COORD;
  else if (strcmp_case(str, "ZERODOPPLER") == 0)
    spec->imageCoordinateType = ZERODOPPLER;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    spec->imageCoordinateType = UNDEF_COORD;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productSpecific.complexImageInfo.imageDataStartWith"));
  if (strcmp_case(str, "EARLYAZNEARRG") == 0)
    spec->imageDataStartWith = EARLYAZNEARRG;
  else if (strcmp_case(str, "EARLYAZFARRG") == 0)
    spec->imageDataStartWith = EARLYAZFARRG;
  else if (strcmp_case(str, "LATEAZNEARRG") == 0)
    spec->imageDataStartWith = LATEAZNEARRG;
  else if (strcmp_case(str, "LATEAZFARRG") == 0)
    spec->imageDataStartWith = LATEAZFARRG;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    spec->imageDataStartWith = UNDEF_DATA_START;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.productSpecific.complexImageInfo.quicklookDataStartWith"));
  if (strcmp(str, "EARLYAZNEARRG") == 0)
    spec->quicklookDataStartWith = EARLYAZNEARRG;
  else if (strcmp_case(str, "EARLYAZFARRG") == 0)
    spec->quicklookDataStartWith = EARLYAZFARRG;
  else if (strcmp_case(str, "LATEAZNEARRG") == 0)
    spec->quicklookDataStartWith = LATEAZNEARRG;
  else if (strcmp_case(str, "LATEAZFARRG") == 0)
    spec->quicklookDataStartWith = LATEAZFARRG;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    spec->quicklookDataStartWith = UNDEF_DATA_START;

  /* FIXME: information about geocoded imagery - can be done later
  spec->geocodedImageInfoFlag = FALSE;
  strcpy(spec->geodeticDatumID, MAGIC_UNSET_STRING);
  strcpy(spec->projectionID, MAGIC_UNSET_STRING);
  strcpy(spec->zoneID, MAGIC_UNSET_STRING);
  spec->projectionCenterLatitude = MAGIC_UNSET_DOUBLE;
  spec->projectionCenterLongitude = MAGIC_UNSET_DOUBLE;
  spec->mapOriginEasting = MAGIC_UNSET_DOUBLE;
  spec->mapOriginNorthing = MAGIC_UNSET_DOUBLE;
  spec->scaleFactor = MAGIC_UNSET_DOUBLE;
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
  spec->elevationDataFlag = FALSE;
  strcpy(spec->elevationDataSource, MAGIC_UNSET_STRING);
  spec->elevationMinimumHeight = MAGIC_UNSET_DOUBLE;
  spec->elevationMeanHeight = MAGIC_UNSET_DOUBLE;
  spec->elevationMaximumHeight = MAGIC_UNSET_DOUBLE;
  spec->incidenceAngleMaskDescriptionFlag = FALSE;
  strcpy(spec->incidenceAnglePixelValueID, MAGIC_UNSET_STRING);
  spec->incidenceAngleImageDataFormat = UNDEF_DATA_FORMAT;
  spec->incidenceAngleImageDataDepth = MAGIC_UNSET_INT;
  spec->incidenceAngleNumberOfRows = MAGIC_UNSET_INT;
  spec->incidenceAngleNumberOfColumns = MAGIC_UNSET_INT;
  spec->incidenceAngleRowSpacing = MAGIC_UNSET_DOUBLE;
  spec->incidenceAngleColumnSpacing = MAGIC_UNSET_DOUBLE;
  */

  // setup
  iso_setup *setup = iso->setup;

  strcpy(setup->orderType, xml_get_string_value(doc,
    "level1Product.setup.orderInfo.orderType"));
  strcpy(setup->processingPriority, xml_get_string_value(doc,
    "level1Product.setup.orderInfo.processingPriority"));
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.orbitAccuracy"));
  if (strcmp_case(str, "PREDICTED") == 0)
    setup->orbitAccuracy = PREDICTED_ORBIT;
  else if (strcmp_case(str, "RESTITUTED") == 0)
    setup->orbitAccuracy = RESTITUTED_ORBIT;
  else if (strcmp_case(str, "PRECISE") == 0)
    setup->orbitAccuracy = PRECISE_ORBIT;
  else if (strcmp_case(str, "TLE") == 0)
    setup->orbitAccuracy = TLE;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->orbitAccuracy = UNDEF_ORBIT_ACC;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.sceneSpecification"));
  if (strcmp_case(str, "FRAME") == 0)
    setup->sceneSpecification = FRAME_SPEC;
  else if (strcmp_case(str, "TIME") == 0)
    setup->sceneSpecification = TIME_SPEC;
  else if (strcmp_case(str, "CENTERCOORDS") == 0)
    setup->sceneSpecification = CENTERCOORDS_SPEC;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->sceneSpecification = UNDEF_SCENE_SPEC;
  setup->frameID = xml_get_int_value(doc,
    "level1Product.setup.orderInfo.orderedScene.frameID");
  /* FIXME: scene specification by start/stop time or center coordinates
  setup->sceneStartTimeUTC.year = MAGIC_UNSET_INT;
  setup->sceneStartTimeUTC.month = MAGIC_UNSET_INT;
  setup->sceneStartTimeUTC.day = MAGIC_UNSET_INT;
  setup->sceneStartTimeUTC.hour = MAGIC_UNSET_INT;
  setup->sceneStartTimeUTC.min = MAGIC_UNSET_INT;
  setup->sceneStartTimeUTC.second = MAGIC_UNSET_DOUBLE;
  setup->sceneStopTimeUTC.year = MAGIC_UNSET_INT;
  setup->sceneStopTimeUTC.month = MAGIC_UNSET_INT;
  setup->sceneStopTimeUTC.day = MAGIC_UNSET_INT;
  setup->sceneStopTimeUTC.hour = MAGIC_UNSET_INT;
  setup->sceneStopTimeUTC.min = MAGIC_UNSET_INT;
  setup->sceneStopTimeUTC.second = MAGIC_UNSET_DOUBLE;
  setup->sceneCenterLatitude = MAGIC_UNSET_DOUBLE;
  setup->sceneCenterLongitude = MAGIC_UNSET_DOUBLE;
  */
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.orderedScene.imagingMode"));
  if (strcmp_case(str, "FINE BEAM") == 0)
    setup->imagingMode = FINE_BEAM;
  else if (strcmp_case(str, "STANDARD BEAM") == 0)
    setup->imagingMode = STANDARD_BEAM;
  else if (strcmp_case(str, "STRIPMAP") == 0)
    setup->imagingMode = STRIPMAP_IMAGE;
  else if (strcmp_case(str, "SCANSAR") == 0)
    setup->imagingMode = SCANSAR_IMAGE;
  else if (strcmp_case(str, "SPOTLIGHT") == 0)
    setup->imagingMode = SPOTLIGHT_IMAGE;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->imagingMode = UNDEF_IMAGE_MODE;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.orderedScene.lookDirection"));
  if (strcmp_case(str, "RIGHT") == 0)
    setup->lookDirection = RIGHT_LOOK;
  else if (strcmp_case(str, "LEFT") == 0)
    setup->lookDirection = LEFT_LOOK;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->lookDirection = UNDEF_LOOK;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.orderedScene.polarizationMode"));
  if (strcmp_case(str, "SINGLE") == 0)
    setup->polarizationMode = SINGLE_POL;
  else if (strcmp_case(str, "DUAL") == 0)
    setup->polarizationMode = DUAL_POL;
  else if (strcmp_case(str, "QUAD") == 0)
    setup->polarizationMode = QUAD_POL;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->polarizationMode = UNDEF_POL_MODE;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.orderedScene.polList.polLayer"));
  if (strcmp_case(str, "HH") == 0)
    setup->polLayer = HH_POL;
  else if (strcmp_case(str, "HV") == 0)
    setup->polLayer = HV_POL;
  else if (strcmp_case(str, "VH") == 0)
    setup->polLayer = VH_POL;
  else if (strcmp_case(str, "VV") == 0)
    setup->polLayer = VV_POL;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->polLayer = UNDEF_POL_LAYER;
  strcpy(setup->elevationBeamConfiguration, xml_get_string_value(doc,
    "level1Product.setup.orderInfo.orderedScene.elevationBeamConfiguration"));
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.orderedScene.productVariant"));
  if (strcmp_case(str, "SLC") == 0)
    setup->productVariant = SLC_PRODUCT;
  else if (strcmp_case(str, "STD") == 0)
    setup->productVariant = STD_PRODUCT;
  else if (strcmp_case(str, "TC") == 0)
    setup->productVariant = TC_PRODUCT;
  else if (strcmp_case(str, "RTC") == 0)
    setup->productVariant = RTC_PRODUCT;
  else if (strcmp_case(str, "GEO") == 0)
    setup->productVariant = GEO_PRODUCT;
  else if (strcmp_case(str, "SSC") == 0)
    setup->productVariant = SSC_PRODUCT;
  else if (strcmp_case(str, "MGD") == 0)
    setup->productVariant = MGD_PRODUCT;
  else if (strcmp_case(str, "GEC") == 0)
    setup->productVariant = GEC_PRODUCT;
  else if (strcmp_case(str, "EEC") == 0)
    setup->productVariant = EEC_PRODUCT;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->productVariant = UNDEF_PRODUCT;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.orderedScene.resolutionVariant"));
  if (strcmp_case(str, "SE") == 0)
    setup->resolutionVariant = SE_RES;
  else if (strcmp_case(str, "RE") == 0)
    setup->resolutionVariant = RE_RES;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->resolutionVariant = UNDEF_RES;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.setup.orderInfo.orderedScene.projection"));
  if (strcmp_case(str, "SLANTRANGE") == 0)
    setup->projection = SLANTRANGE_PROJ;
  else if (strcmp_case(str, "GROUNDRANGE") == 0)
    setup->projection = GROUNDRANGE_PROJ;
  else if (strcmp_case(str, "MAP") == 0)
    setup->projection = MAP_PROJ;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    setup->projection = UNDEF_PROJ;
  strcpy(setup->logicalDataTakeID, xml_get_string_value(doc,
    "level1Product.setup.inputData.inputDataTakeID"));
  strcpy(setup->level0ProductID, xml_get_string_value(doc,
    "level1Product.setup.inputData.level0ProductID"));
  strcpy(str, xml_get_string_value(doc,
    "level1Product.setup.inputData.L0SARGenerationTimeUTC"));
  str2dateTime(str, &setup->L0SARGenerationTimeUTC);
  setup->numProcessingSteps = 
    xml_get_children_count(doc, "level1Product.setup.processingSteps");
  setup->processingStep = 
    (iso_procStep *) MALLOC(sizeof(iso_procStep)*setup->numProcessingSteps);
  for (ii=0; ii<setup->numProcessingSteps; ii++) {
    sprintf(str, 
	    "level1Product.setup.processingSteps[%d].software.softwareID", ii);
    strcpy(setup->processingStep[ii].softwareID, 
	   xml_get_string_value(doc, str));
    sprintf(str, 
	    "level1Product.setup.processingSteps[%d].software.softwareVersion",
	    ii);
    strcpy(setup->processingStep[ii].softwareVersion, 
	   xml_get_string_value(doc, str));
    sprintf(element, 
	    "level1Product.setup.processingSteps[%d].software.description", ii);
    strcpy(setup->processingStep[ii].description, 
	   xml_get_string_value(doc, element));
    sprintf(element, 
	    "level1Product.setup.processingSteps[%d].software.algorithm", ii);
    strcpy(setup->processingStep[ii].algorithm, 
	   xml_get_string_value(doc, element));
    sprintf(element, 
	    "level1Product.setup.processingSteps[%d].software.processingTimeUTC",
	    ii);
    strcpy(str, xml_get_string_value(doc, element));
    str2dateTime(str, &setup->processingStep[ii].processingTimeUTC);
    sprintf(element,
	    "level1Product.setup.processingSteps[%d].software.processingLevel",
	    ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "PRE-PROCESSING") == 0)
      setup->processingStep[ii].processingLevel = PRE_PROCESSING;
    else if (strcmp_case(str, "LEVEL ZERO") == 0)
      setup->processingStep[ii].processingLevel = LEVEL_ZERO;
    else if (strcmp_case(str, "LEVEL ONE") == 0)
      setup->processingStep[ii].processingLevel = LEVEL_ONE;
    else if (strcmp_case(str, "LEVEL TWO") == 0)
      setup->processingStep[ii].processingLevel = LEVEL_TWO;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      setup->processingStep[ii].processingLevel = UNDEF_PROC_LEVEL;
  }

  // processing
  iso_processing *proc = iso->processing;

  strcpy(proc->dopplerBasebandEstimationMethod, xml_get_string_value(doc,
    "level1Product.processing.doppler.dopplerBasebandEstimationMethod"));
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.doppler.dopplerCentroidCoordinateType"));
  if (strcmp_case(str, "RAW") == 0)
    proc->dopplerCentroidCoordinateType = RAW_COORD;
  else if (strcmp_case(str, "ZERODOPPLER") == 0)
    proc->dopplerCentroidCoordinateType = ZERODOPPLER;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    proc->dopplerCentroidCoordinateType = UNDEF_COORD;
  proc->doppler = 
    (iso_dopplerCentroid *) MALLOC(sizeof(iso_dopplerCentroid)*numLayers);
  for (ii=0; ii<numLayers; ii++) {
    sprintf(element, "level1Product.processing.doppler.dopplerCentroid[%d].polLayer", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "HH") == 0)
      proc->doppler[ii].polLayer = HH_POL;
    else if (strcmp_case(str, "HV") == 0)
      proc->doppler[ii].polLayer = HV_POL;
    else if (strcmp_case(str, "VH") == 0)
      proc->doppler[ii].polLayer = VH_POL;
    else if (strcmp_case(str, "VV") == 0)
      proc->doppler[ii].polLayer = VV_POL;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      proc->doppler[ii].polLayer = UNDEF_POL_LAYER;
    sprintf(element, "level1Product.processing.doppler.dopplerCentroid[%d].numberOfBlocks", ii);
    proc->doppler[ii].numberOfBlocks = xml_get_int_value(doc, element);
    sprintf(element, "level1Product.processing.doppler.dopplerCentroid[%d].numberOfRejectedBlocks", ii);
    proc->doppler[ii].numberOfRejectedBlocks = xml_get_int_value(doc, element);
    sprintf(element, "level1Product.processing.doppler.dopplerCentroid[%d].numberOfDopplerRecords", ii);
    proc->doppler[ii].numberOfDopperRecords = xml_get_int_value(doc, element);
    sprintf(element, "level1Product.processing.doppler.dopplerCentroid[%d].dopplerEstimate.timeUTC", ii);
    strcpy(str, xml_get_string_value(doc, element));
    str2dateTime(str, &proc->doppler[ii].timeUTC);
    sprintf(element, "level1Product.processing.doppler.dopplerCentroid[%d].dopplerEstimate.dopplerAtMidRange", ii);
    proc->doppler[ii].dopplerAtMidRange = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.processing.doppler.dopplerCentroid[%d].dopplerEstimate.polynomialDegree", ii);    
    proc->doppler[ii].polynomialDegree = xml_get_double_value(doc, element);
    int degree = proc->doppler[ii].polynomialDegree;
    proc->doppler[ii].coefficient = 
      (double *) MALLOC(sizeof(double)*(degree+1));
    for (kk=0; kk<=degree; kk++) {
      sprintf(element, "level1Product.processing.doppler.dopplerCentroid[%d].dopplerEstimate.basebandDoppler.coefficient[%d]", ii, kk);
      proc->doppler[ii].coefficient[kk] = xml_get_double_value(doc, element);
    }
  }
  proc->processingParameter = 
    (iso_processingParameter *) MALLOC(sizeof(iso_processingParameter));
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.processing.processingParameter.processingInfoCoordinateType"));
  if (strcmp_case(str, "RAW") == 0)
    proc->processingParameter[0].processingInfoCoordinateType = RAW_COORD;
  else if (strcmp_case(str, "ZERODOPPLER") == 0)
    proc->processingParameter[0].processingInfoCoordinateType = ZERODOPPLER;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    proc->processingParameter[0].processingInfoCoordinateType = UNDEF_COORD;
  proc->processingParameter[0].rangeLooks = xml_get_double_value(doc, 
    "level1Product.processing.processingParameter.rangeLooks");
  proc->processingParameter[0].azimuthLooks = xml_get_double_value(doc, 
    "level1Product.processing.processingParameter.azimuthLooks");
  proc->processingParameter[0].rangeLookBandwidth = xml_get_double_value(doc, 
    "level1Product.processing.processingParameter.rangeLookBandwidth");
  proc->processingParameter[0].azimuthLookBandwidth = xml_get_double_value(doc, 
    "level1Product.processing.processingParameter.azimuthLookBandwidth");
  proc->processingParameter[0].totalProcessedRangeBandwidth = 
    xml_get_double_value(doc, 
    "level1Product.processing.processingParameter.totalProcessedRangeBandwidth");
  proc->processingParameter[0].totalProcessedAzimuthBandwidth = 
    xml_get_double_value(doc, 
    "level1Product.processing.processingParameter.totalProcessedAzimuthBandwidth");
  proc->processingParameter[0].chirpRate = xml_get_double_value(doc, 
    "level1Product.processing.processingParameter.chirpRate");
  proc->processingParameter[0].pulseDuration = xml_get_double_value(doc, 
    "level1Product.processing.processingParameter.pulseDuration");
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.chirpReplicaUsedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->chirpReplicaUsedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.geometricDopplerUsedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->geometricDopplerUsedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.azimuthPatternCorrectedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->azimuthPatternCorrectedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.elevationPatternCorrectedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->elevationPatternCorrectedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.detectedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->detectedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.multiLookedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->multiLookedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.polarimetricProcessedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->polarimetricProcessedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.terrainCorrectedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->terrainCorrectedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.layoverShadowMaskGeneratedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->layoverShadowMaskGeneratedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.geocodedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->geocodedFlag = TRUE;
  strcpy(str, xml_get_string_value(doc,
    "level1Product.processing.processingFlags.nominalProcessingPerformedFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    proc->nominalProcessingPerformedFlag = TRUE;

  // instrument
  iso_instrument *inst = iso->instrument;

  strcpy(str, xml_get_string_value(doc,
    "level1Product.instrument.instrumentInfoCoordinateType"));
  if (strcmp_case(str, "RAW") == 0)
    inst->instrumentInfoCoordinateType = RAW_COORD;
  else if (strcmp_case(str, "ZERODOPPLER") == 0)
    inst->instrumentInfoCoordinateType = ZERODOPPLER;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    inst->instrumentInfoCoordinateType = UNDEF_COORD;
  inst->centerFrequency = xml_get_double_value(doc,
    "level1Product.instrument.radarParameters.centerFrequency");
  inst->numSettings = numLayers;
  inst->settings = (iso_settings *) MALLOC(sizeof(iso_settings)*numLayers);
  for (ii=0; ii<numLayers; ii++) {
    sprintf(element, "level1Product.instrument.settings[%d].polLayer", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "HH") == 0)
      inst->settings[ii].polLayer = HH_POL;
    else if (strcmp_case(str, "HV") == 0)
      inst->settings[ii].polLayer = HV_POL;
    else if (strcmp_case(str, "VH") == 0)
      inst->settings[ii].polLayer = VH_POL;
    else if (strcmp_case(str, "VV") == 0)
      inst->settings[ii].polLayer = VV_POL;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      inst->settings[ii].polLayer = UNDEF_POL_LAYER;
    sprintf(element, "level1Product.instrument.settings[%d].beamID", ii);
    strcpy(inst->settings[ii].beamID, xml_get_string_value(doc, element));
    sprintf(element, "level1Product.instrument.settings[%d].rxBandwidth", ii);
    inst->settings[ii].rxBandwidth = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.instrument.settings[%d].RSF", ii);
    inst->settings[ii].rsf = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.instrument.settings[%d].numberOfPRFChanges", ii);
    inst->settings[ii].numberOfPRFChanges = xml_get_int_value(doc, element);
    sprintf(element, "level1Product.instrument.settings[%d].numberOfEchoWindowPositionChanges", ii);
    inst->settings[ii].numberOfEchoWindowPositionChanges = 
      xml_get_int_value(doc, element);
    sprintf(element, "level1Product.instrument.settings[%d].numberOfEchoWindowLengthChanges", ii);
    inst->settings[ii].numberOfEchoWindowLengthChanges = 
      xml_get_int_value(doc, element);
    sprintf(element, "level1Product.instrument.settings[%d].numberOfSettingRecords", ii);
    inst->settings[ii].numberOfSettingRecords = xml_get_int_value(doc, element);
    int numSets = inst->settings[ii].numberOfSettingRecords;
    inst->settings[ii].settingRecord = 
      (iso_settingRecord *) MALLOC(sizeof(iso_settingRecord)*numSets);
    iso_settingRecord *rec = inst->settings[ii].settingRecord;
    for (kk=0; kk<inst->settings[ii].numberOfSettingRecords; kk++) {
      sprintf(element, "level1Product.instrument.settings[%d].settingRecords[%d].dataSegment.startTimeUTC", ii, kk);
      strcpy(str, xml_get_string_value(doc, element));
      str2dateTime(str, &rec[kk].startTimeUTC);
      sprintf(element, "level1Product.instrument.settings[%d].settingRecords[%d].dataSegment.stopTimeUTC", ii, kk);
      strcpy(str, xml_get_string_value(doc, element));
      str2dateTime(str, &rec[kk].stopTimeUTC);
      sprintf(element, "level1Product.instrument.settings[%d].settingRecords[%d].dataSegment.numberOfRows", ii, kk);
      rec[kk].numberOfRows = xml_get_int_value(doc, element);
      sprintf(element, "level1Product.instrument.settings[%d].settingRecords[%d].PRF", ii, kk);
      rec[kk].prf = xml_get_double_value(doc, element);
      sprintf(element, "level1Product.instrument.settings[%d].settingRecords[%d].echoWindowPosition", ii, kk);
      rec[kk].echoWindowPosition = xml_get_double_value(doc, element);
      sprintf(element, "level1Product.instrument.settings[%d].settingRecords[%d].echoWindowLength", ii, kk);
      rec[kk].echoWindowLength = xml_get_double_value(doc, element);
      sprintf(element, "level1Product.instrument.settings[%d].settingRecords[%d].pulseType", ii, kk);
      strcpy(rec[kk].pulseType, xml_get_string_value(doc, element));
    }
  }
  
  // platform
  iso_platform *platform = iso->platform;

  strcpy(str, xml_get_string_value(doc, 
    "level1Product.platform.orbit.orbitHeader.sensor"));
  if (strcmp_case(str, "PREDICTED SENSOR") == 0)
    platform->sensor = PREDICTED_SENSOR;
  else if (strcmp_case(str, "SINGLE GPS") == 0)
    platform->sensor = SINGLE_GPS;
  else if (strcmp_case(str, "DIFFERENTIAL GPS") == 0)
    platform->sensor = DIFFERENTIAL_GPS;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    platform->sensor = UNDEF_ORBIT_SENSOR;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.platform.orbit.orbitHeader.sensor"));
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.platform.orbit.orbitHeader.accuracy"));
  if (strcmp_case(str, "PREDICTED") == 0)
    platform->accuracy = PREDICTED_ORBIT;
  else if (strcmp_case(str, "RESTITUTED") == 0)
    platform->accuracy = RESTITUTED_ORBIT;
  else if (strcmp_case(str, "PRECISE") == 0)
    platform->accuracy = PRECISE_ORBIT;
  else if (strcmp_case(str, "TLE") == 0)
    platform->accuracy = TLE;
  else if (strcmp_case(str, "UNDEFINED") == 0)
    platform->accuracy = UNDEF_ORBIT_ACC;
  platform->numStateVectors = xml_get_int_value(doc, 
    "level1Product.platform.orbit.orbitHeader.numStateVectors");
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.platform.orbit.orbitHeader.firstStateTimeUTC"));
  str2dateTime(str, &platform->firstStateTimeUTC);
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.platform.orbit.orbitHeader.lastStateTimeUTC"));
  str2dateTime(str, &platform->lastStateTimeUTC);
  strcpy(platform->stateVectorRefFrame, xml_get_string_value(doc, 
    "level1Product.platform.orbit.orbitHeader.stateVectorRefFrame"));
  platform->stateVectorTimeSpacing = xml_get_double_value(doc, 
    "level1Product.platform.orbit.orbitHeader.stateVectorTimeSpacing");
  platform->stateVec = 
    (iso_stateVec *) MALLOC(sizeof(iso_stateVec)*platform->numStateVectors);
  for (ii=0; ii<platform->numStateVectors; ii++) {
    sprintf(element, "level1Product.platform.orbit.stateVec[%d].timeUTC", ii);
    strcpy(str, xml_get_string_value(doc, element));
    str2dateTime(str, &platform->stateVec[ii].timeUTC);
    sprintf(element, "level1Product.platform.orbit.stateVec[%d].posX", ii);
    strcpy(str, xml_get_string_value(doc, element));
    platform->stateVec[ii].posX = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.platform.orbit.stateVec[%d].posY", ii);
    strcpy(str, xml_get_string_value(doc, element));
    platform->stateVec[ii].posY = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.platform.orbit.stateVec[%d].posZ", ii);
    strcpy(str, xml_get_string_value(doc, element));
    platform->stateVec[ii].posZ = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.platform.orbit.stateVec[%d].velX", ii);
    strcpy(str, xml_get_string_value(doc, element));
    platform->stateVec[ii].velX = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.platform.orbit.stateVec[%d].velY", ii);
    strcpy(str, xml_get_string_value(doc, element));
    platform->stateVec[ii].velY = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.platform.orbit.stateVec[%d].velZ", ii);
    strcpy(str, xml_get_string_value(doc, element));
    platform->stateVec[ii].velZ = xml_get_double_value(doc, element);
  }

  // productQuality
  iso_productQuality *quality = iso->productQuality;

  quality->rawDataQuality = 
    (iso_rawDataQuality *) MALLOC(sizeof(iso_rawDataQuality)*numLayers);
  for (ii=0; ii<numLayers; ii++) {
    sprintf(element, "level1Product.productQuality.rawDataQuality[%d].polLayer", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "HH") == 0)
      quality->rawDataQuality[ii].polLayer = HH_POL;
    else if (strcmp_case(str, "HV") == 0)
      quality->rawDataQuality[ii].polLayer = HV_POL;
    else if (strcmp_case(str, "VH") == 0)
      quality->rawDataQuality[ii].polLayer = VH_POL;
    else if (strcmp_case(str, "VV") == 0)
      quality->rawDataQuality[ii].polLayer = VV_POL;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      quality->rawDataQuality[ii].polLayer = UNDEF_POL_LAYER;
    sprintf(element, "level1Product.productQuality.rawDataQuality[%d].beamID", ii);
    strcpy(quality->rawDataQuality[ii].beamID, 
	   xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productQuality.rawDataQuality[%d].numGaps", ii);
    quality->rawDataQuality[ii].numGaps = xml_get_int_value(doc, element);
    int numGaps = quality->rawDataQuality[ii].numGaps;
    if (numGaps > 0)
      quality->rawDataQuality[ii].gap = 
	(iso_gap *) MALLOC(sizeof(iso_gap)*numGaps);
    else
      quality->rawDataQuality[ii].gap = NULL;
    for (kk=0; kk<numGaps; kk++) {
      sprintf(element, "level1Product.productQuality.rawDataQuality[%d].gap[%d].start", ii, kk);
      quality->rawDataQuality[ii].gap[kk].start = 
	xml_get_long_value(doc, element);
      sprintf(element, "level1Product.productQuality.rawDataQuality[%d].gap[%d].length", ii, kk);
      quality->rawDataQuality[ii].gap[kk].length = 
	xml_get_int_value(doc, element);
      sprintf(element, "level1Product.productQuality.rawDataQuality[%d].gap[%d].fill", ii, kk);      
      strcpy(str, xml_get_string_value(doc, element));
      if (strcmp_case(str, "RANDOM") == 0)
	quality->rawDataQuality[ii].gap[kk].fill = RANDOM_FILL;
      else if (strcmp_case(str, "ZERO") == 0)
	quality->rawDataQuality[ii].gap[kk].fill = ZERO_FILL;
      else if (strcmp_case(str, "UNDEFINED") == 0)
	quality->rawDataQuality[ii].gap[kk].fill = UNDEF_FILL;
    }
    sprintf(element, "level1Product.productQuality.rawDataQuality[%d].gapSignificanceFlag", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "TRUE") == 0)
      quality->rawDataQuality[ii].gapSignificanceFlag = TRUE;
    else if (strcmp_case(str, "FALSE") == 0)
      quality->rawDataQuality[ii].gapSignificanceFlag = FALSE;
    sprintf(element, "level1Product.productQuality.rawDataQuality[%d].missingLinesSignificanceFlag", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "TRUE") == 0)
      quality->rawDataQuality[ii].missingLinesSignificanceFlag = TRUE;
    else if (strcmp_case(str, "FALSE") == 0)
      quality->rawDataQuality[ii].missingLinesSignificanceFlag = FALSE;
    sprintf(element, "level1Product.productQuality.rawDataQuality[%d].bitErrorSignificanceFlag", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "TRUE") == 0)
      quality->rawDataQuality[ii].bitErrorSignificanceFlag = TRUE;
    else if (strcmp_case(str, "FALSE") == 0)
      quality->rawDataQuality[ii].bitErrorSignificanceFlag = FALSE;
    sprintf(element, "level1Product.productQuality.rawDataQuality[%d].timeReconstructionSignificanceFlag", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "TRUE") == 0)
      quality->rawDataQuality[ii].timeReconstructionSignificanceFlag = TRUE;
    else if (strcmp_case(str, "FALSE") == 0)
      quality->rawDataQuality[ii].timeReconstructionSignificanceFlag = FALSE;
  }
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.productQuality.processingParameterQuality.dopplerAmbiguityNotZeroFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    quality->dopplerAmbiguityNotZeroFlag = TRUE;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.productQuality.processingParameterQuality.dopplerOutsideLimitsFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    quality->dopplerOutsideLimitsFlag = FALSE;
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.productQuality.processingParameterQuality.geolocationQualityFlag"));
  if (strcmp_case(str, "TRUE") == 0)
    quality->geolocationQualityLowFlag = FALSE;
  quality->imageDataQuality = 
    (iso_imageDataQuality *) MALLOC(sizeof(iso_imageDataQuality));
  for (ii=0; ii<numLayers; ii++) {
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].polLayer", ii);
    strcpy(str, xml_get_string_value(doc, element));
    if (strcmp_case(str, "HH") == 0)
      quality->imageDataQuality[ii].polLayer = HH_POL;
    else if (strcmp_case(str, "HV") == 0)
      quality->imageDataQuality[ii].polLayer = HV_POL;
    else if (strcmp_case(str, "VH") == 0)
      quality->imageDataQuality[ii].polLayer = VH_POL;
    else if (strcmp_case(str, "VV") == 0)
      quality->imageDataQuality[ii].polLayer = VV_POL;
    else if (strcmp_case(str, "UNDEFINED") == 0)
      quality->imageDataQuality[ii].polLayer = UNDEF_POL_LAYER;
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].beamID", ii);
    strcpy(quality->imageDataQuality[ii].beamID, 
	   xml_get_string_value(doc, element));
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].imageDataStatistics.minValue", ii);
    quality->imageDataQuality[ii].min = xml_get_double_value(doc, element);    
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].imageDataStatistics.maxValue", ii);
    quality->imageDataQuality[ii].max = xml_get_double_value(doc, element);    
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].imageDataStatistics.meanValue", ii);
    quality->imageDataQuality[ii].mean = xml_get_double_value(doc, element);    
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].imageDataStatistics.standardDeviation", ii);
    quality->imageDataQuality[ii].stdDev = xml_get_double_value(doc, element);
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].imageDataStatistics.missingLines", ii);
    quality->imageDataQuality[ii].missingLines = 
      xml_get_int_value(doc, element);
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].imageDataStatistics.bitErrorRate", ii);
    quality->imageDataQuality[ii].bitErrorRate = 
      xml_get_double_value(doc, element);
    sprintf(element, "level1Product.productQuality.imageDataQuality[%d].imageDataStatistics.noDataValue", ii);
    quality->imageDataQuality[ii].noData = xml_get_double_value(doc, element);
  }
  quality->gapDefinition = xml_get_int_value(doc, 
    "level1Product.productQuality.limits.rawData.gapDefinition");
  quality->gapPercentageLimit = xml_get_double_value(doc, 
    "level1Product.productQuality.limits.rawData.gapPercentageLimit");
  quality->missingLinePercentageLimit = xml_get_double_value(doc, 
    "level1Product.productQuality.limits.rawData.missingLinePercentageLimit");
  quality->bitErrorLimit = xml_get_double_value(doc, 
    "level1Product.productQuality.limits.rawData.bitErrorLimit");
  quality->timeReconstructionPercentageLimit = xml_get_double_value(doc, 
    "level1Product.productQuality.limits.rawData.timeReconstructionPercentageLimit");
  quality->dopplerCentroidLimit = xml_get_double_value(doc, 
    "level1Product.productQuality.limits.processing.dopplerCentroidLimit");
  quality->geolocationQualityLimit = xml_get_double_value(doc, 
    "level1Product.productQuality.limits.processing.geolocationQualityLimit");
  strcpy(str, xml_get_string_value(doc, 
    "level1Product.productQuality.instrumentStateRemark"));
  if (strcmp(str, MAGIC_UNSET_STRING) != 0) {
    quality->instrumentStateRemark = (char *) MALLOC(sizeof(char)*1024);
    strcpy(quality->instrumentStateRemark, str);
  }
		  
  // Clean up
  xmlFreeDoc(doc);
  xmlCleanupParser();

  return iso;
}
