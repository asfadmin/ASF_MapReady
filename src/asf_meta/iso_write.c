#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include "asf_iso_meta.h"

static void dateTime2str(iso_dateTime timeUTC, char *str)
{
  int year = timeUTC.year;
  int month = timeUTC.month;
  int day = timeUTC.day;
  int hour = timeUTC.hour;
  int min = timeUTC.min;
  double sec = timeUTC.second;

  // UTC time stamp: 2008-03-13T22:19:55.140975Z
  if (year < 0 || month < 0 || day < 0 || hour < 0 || min < 0 || sec < 0.0)
    strcpy(str, MAGIC_UNSET_STRING);
  else
    sprintf(str, "%4d-%02d-%02dT%02d:%02d:%02.6fZ", 
	    year, month, day, hour, min, sec);
}

static xmlChar *iStr(int number)
{
  char *str = (char *) MALLOC(sizeof(char)*30);
  sprintf(str, "%i", number);
  return BAD_CAST str;
}

static xmlChar *ldStr(long number)
{
  char *str = (char *) MALLOC(sizeof(char)*30);
  sprintf(str, "%ld", number);
  return BAD_CAST str;
}

static xmlChar *fStr(float number)
{
  char *str = (char *) MALLOC(sizeof(char)*30);
  sprintf(str, "%g", number);
  return BAD_CAST str;
}

static xmlChar *f3Str(float number)
{
  char *str = (char *) MALLOC(sizeof(char)*30);
  sprintf(str, "%.3f", number);
  return BAD_CAST str;
}

static xmlChar *f5Str(float number)
{
  char *str = (char *) MALLOC(sizeof(char)*30);
  sprintf(str, "%.5f", number);
  return BAD_CAST str;
}

static xmlChar *lfStr(double number)
{
  char *str = (char *) MALLOC(sizeof(char)*30);
  sprintf(str, "%g", number);
  return BAD_CAST str;
}

static xmlChar *lf3Str(double number)
{
  char *str = (char *) MALLOC(sizeof(char)*30);
  sprintf(str, "%.3f", number);
  return BAD_CAST str;
}

static xmlChar *lf5Str(double number)
{
  char *str = (char *) MALLOC(sizeof(char)*30);
  sprintf(str, "%.5f", number);
  return BAD_CAST str;
}

static xmlChar *bStr(int number)
{
  char *str = (char *) MALLOC(sizeof(char)*10);
  if (number == 1)
    strcpy(str, "TRUE");
  else if (number == 0)
    strcpy(str, "FALSE");
  return BAD_CAST str;
}

void iso_meta_write(iso_meta *iso, const char *outFile)
{
  unsigned long ii, kk;
  char str[30];
  
  // Set up 
  xmlDoc *doc = xmlNewDoc(BAD_CAST "1.0");
  xmlNode *root = xmlNewNode(NULL, BAD_CAST "level1Product");
  xmlNs *ns = xmlNewNs(root, 
		       BAD_CAST "http://www.w3.org/2001/XMLSchema-instance",
		       BAD_CAST "xsi");
  xmlNewNsProp(root, ns, BAD_CAST "noNamespaceSchemaLocation",
	       BAD_CAST "iso_meta.xsd");
  xmlDocSetRootElement(doc, root);
  
  // Generate the tree from iso structure
  xmlNodePtr unit, node3, node2, node, parent, section;
  
  // General Header
  iso_generalHeader *header = iso->generalHeader;
  section = xmlNewChild(root, NULL, BAD_CAST "generalHeader", NULL);
  xmlNewProp(section, BAD_CAST "fileName", BAD_CAST outFile);
  xmlNewProp(section, BAD_CAST "fileVersion", BAD_CAST "1.0");
  xmlNewProp(section, BAD_CAST "status", BAD_CAST "PRELIMINARY");
  xmlNewChild(section, NULL, BAD_CAST "itemName", BAD_CAST header->itemName);
  xmlNewChild(section, NULL, BAD_CAST "mission", BAD_CAST header->mission);
  xmlNewChild(section, NULL, BAD_CAST "source", BAD_CAST header->source);
  xmlNewChild(section, NULL, BAD_CAST "destination", 
	      BAD_CAST header->destination);
  xmlNewChild(section, NULL, BAD_CAST "generationSystem", 
	      BAD_CAST header->generationSystem);
  dateTime2str(header->generationTime, str);
  xmlNewChild(section, NULL, BAD_CAST "generationTime", BAD_CAST str);
  if (header->referenceDocument)
    xmlNewChild(section, NULL, BAD_CAST "referenceDocument", 
		BAD_CAST header->referenceDocument);
  if (header->revision)
    xmlNewChild(section, NULL, BAD_CAST "revision", BAD_CAST header->revision);
  if (header->revisionComment)
    xmlNewChild(section, NULL, BAD_CAST "revisionComment", 
		BAD_CAST header->revisionComment);
  
  // Product Components
  iso_productComponents *comps = iso->productComponents;
  section = xmlNewChild(root, NULL, BAD_CAST "productComponents", NULL);
  if (comps->annotation) {
    parent = xmlNewChild(section, NULL, BAD_CAST "annotation", NULL);
    for (ii=0; ii<comps->numAnnotations; ii++) {
      if (comps->annotation[ii].type == MAIN_TYPE)
	strcpy(str, "MAIN");
      else if (comps->annotation[ii].type == GEOREF_TYPE)
	strcpy(str, "GEOREF");
      else if (comps->annotation[ii].type == GEOCODE_TYPE)
	strcpy(str, "GEOCODE");
      else if (comps->annotation[ii].type == OTHER_TYPE)
	strcpy(str, "OTHER");
      else if (comps->annotation[ii].type == UNDEF_TYPE)
	strcpy(str, "UNDEFINED");
      xmlNewChild(parent, NULL, BAD_CAST "type", BAD_CAST str);
      node = xmlNewChild(parent, NULL, BAD_CAST "file", NULL);
      parent = node;
      node = xmlNewChild(parent, NULL, BAD_CAST "location", NULL);
      xmlNewChild(node, NULL, BAD_CAST "host", 
		  BAD_CAST comps->annotation[ii].file.host);
      xmlNewChild(node, NULL, BAD_CAST "path", 
		  BAD_CAST comps->annotation[ii].file.path);
      xmlNewChild(node, NULL, BAD_CAST "filename", 
		  BAD_CAST comps->annotation[ii].file.name);
      xmlNewChild(parent, NULL, BAD_CAST "size", 
		  ldStr(comps->annotation[ii].file.size));
    }
  }
  if (comps->imageData) {
    for (ii=0; ii<comps->numLayers; ii++) {
      parent = xmlNewChild(section, NULL, BAD_CAST "imageData", NULL);
      xmlNewProp(parent, BAD_CAST "layerIndex", ldStr(ii+1));
      if (comps->imageData[ii].polLayer == HH_POL)
	strcpy(str, "HH");
      else if (comps->imageData[ii].polLayer == HV_POL)
	strcpy(str, "HV");
      else if (comps->imageData[ii].polLayer == VH_POL)
	strcpy(str, "VH");
      else if (comps->imageData[ii].polLayer == VV_POL)
	strcpy(str, "VV");
      else if (comps->imageData[ii].polLayer == UNDEF_POL_LAYER)
	strcpy(str, "UNDEFINED");
      xmlNewChild(parent, NULL, BAD_CAST "polLayer", BAD_CAST str);
      node = xmlNewChild(parent, NULL, BAD_CAST "file", NULL);
      node2 = xmlNewChild(node, NULL, BAD_CAST "location", NULL);
      xmlNewChild(node2, NULL, BAD_CAST "host", 
		  BAD_CAST comps->imageData[ii].file.host);
      xmlNewChild(node2, NULL, BAD_CAST "path",
		  BAD_CAST comps->imageData[ii].file.path);
      xmlNewChild(node2, NULL, BAD_CAST "filename",
		  BAD_CAST comps->imageData[ii].file.name);
      xmlNewChild(node, NULL, BAD_CAST "size", 
		  ldStr(comps->imageData[ii].file.size));
    }
  }
  if (comps->auxRasterFiles) {
    // FIXME: fill as needed
  }
  if (comps->quicklooks) {
    for (ii=0; ii<comps->numLayers; ii++) {
      parent = xmlNewChild(section, NULL, BAD_CAST "quicklooks", NULL);
      xmlNewProp(parent, BAD_CAST "layerIndex", ldStr(ii+1));
      if (comps->quicklooks[ii].polLayer == HH_POL)
	strcpy(str, "HH");
      else if (comps->quicklooks[ii].polLayer == HV_POL)
	strcpy(str, "HV");
      else if (comps->quicklooks[ii].polLayer == VH_POL)
	strcpy(str, "VH");
      else if (comps->quicklooks[ii].polLayer == VV_POL)
	strcpy(str, "VV");
      else if (comps->quicklooks[ii].polLayer == UNDEF_POL_LAYER)
	strcpy(str, "UNDEFINED");
      xmlNewChild(parent, NULL, BAD_CAST "polLayer", BAD_CAST str);
      node = xmlNewChild(parent, NULL, BAD_CAST "file", NULL);
      node2 = xmlNewChild(node, NULL, BAD_CAST "location", NULL);
      xmlNewChild(node2, NULL, BAD_CAST "host", 
		  BAD_CAST comps->quicklooks[ii].file.host);
      xmlNewChild(node2, NULL, BAD_CAST "path",
		  BAD_CAST comps->quicklooks[ii].file.path);
      xmlNewChild(node2, NULL, BAD_CAST "filename",
		  BAD_CAST comps->quicklooks[ii].file.name);
      xmlNewChild(node, NULL, BAD_CAST "size", 
		  ldStr(comps->quicklooks[ii].file.size));
    }
  }

  /*
    if (comps->compositeQuicklook) {
    parent = xmlNewChild(section, NULL, BAD_CAST "compositeQuicklook", NULL);
    node = xmlNewChild(parent, NULL, BAD_CAST "file", NULL);
    parent = node;
    node = xmlNewChild(parent, NULL, BAD_CAST "location", NULL);
    xmlNewChild(node, NULL, BAD_CAST "host", 
    BAD_CAST comps->compositeQuicklook.host);
    xmlNewChild(node, NULL, BAD_CAST "path", 
    BAD_CAST comps->compositeQuicklook.path);
    xmlNewChild(node, NULL, BAD_CAST "filename", 
    BAD_CAST comps->compositeQuicklook.name);
    xmlNewChild(parent, NULL, BAD_CAST "size", 
    ldStr(comps->compositeQuicklook.size));
    }
  */
  parent = xmlNewChild(section, NULL, BAD_CAST "browseImage", NULL);
  node = xmlNewChild(parent, NULL, BAD_CAST "file", NULL);
  parent = node;
  node = xmlNewChild(parent, NULL, BAD_CAST "location", NULL);
  xmlNewChild(node, NULL, BAD_CAST "host", BAD_CAST comps->browseImage.host);
  xmlNewChild(node, NULL, BAD_CAST "path", BAD_CAST comps->browseImage.path);
  xmlNewChild(node, NULL, BAD_CAST "filename", 
	      BAD_CAST comps->browseImage.name);
  xmlNewChild(parent, NULL, BAD_CAST "size", ldStr(comps->browseImage.size));
  parent = xmlNewChild(section, NULL, BAD_CAST "mapPlot", NULL);
  node = xmlNewChild(parent, NULL, BAD_CAST "file", NULL);
  parent = node;
  node = xmlNewChild(parent, NULL, BAD_CAST "location", NULL);
  xmlNewChild(node, NULL, BAD_CAST "host", BAD_CAST comps->mapPlot.host);
  xmlNewChild(node, NULL, BAD_CAST "path", BAD_CAST comps->mapPlot.path);
  xmlNewChild(node, NULL, BAD_CAST "filename", BAD_CAST comps->mapPlot.name);
  xmlNewChild(parent, NULL, BAD_CAST "size", ldStr(comps->mapPlot.size));
  
  // Product Info
  iso_productInfo *info = iso->productInfo;
  section = xmlNewChild(root, NULL, BAD_CAST "productInfo", NULL);
  parent = xmlNewChild(section, NULL, BAD_CAST "generationInfo", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "logicalProductID", 
	      BAD_CAST info->logicalProductID);
  xmlNewChild(parent, NULL, BAD_CAST "receivingStation", 
	      BAD_CAST info->receivingStation);
  xmlNewChild(parent, NULL, BAD_CAST "level0ProcessingFacility", 
	      BAD_CAST info->level0ProcessingFacility);
  xmlNewChild(parent, NULL, BAD_CAST "level1ProcessingFacility",
	      BAD_CAST info->level1ProcessingFacility);
  if (info->groundOperationsType == OPERATIONAL)
    strcpy(str, "OPERATIONAL");
  else if (info->groundOperationsType == PREOPERATIONAL)
    strcpy(str, "PRE-OPERATIONAL");
  else if (info->groundOperationsType == INSTRUMENT)
    strcpy(str, "INSTRUMENT");
  else if (info->groundOperationsType == TEST_OPS)
    strcpy(str, "TEST");
  else if (info->groundOperationsType == UNDEF_OPS)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "groundOperationsType", BAD_CAST str);
  xmlNewChild(parent, NULL, BAD_CAST "deliveryInfo", 
	      BAD_CAST info->deliveryInfo);
  xmlNewChild(parent, NULL, BAD_CAST "copyrightInfo", 
	      BAD_CAST info->copyrightInfo);
  node = xmlNewChild(parent, NULL, BAD_CAST "qualityInfo", NULL);
  if (info->qualityInspection == AUTO_APPROVED)
    strcpy(str, "AUTO APPROVED");
  else if (info->qualityInspection == OPERATOR_APPROVED)
    strcpy(str, "OPERATOR APPROVED");
  else if (info->qualityInspection == NOT_APPROVED)
    strcpy(str, "NOT APPROVED");
  else if (info->qualityInspection == UNDEF_QUALITY)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "qualityInspection", BAD_CAST str);
  if (info->qualityRemark)
    xmlNewChild(node, NULL, BAD_CAST "qualityRemark", 
		BAD_CAST info->qualityRemark);
  
  parent = xmlNewChild(section, NULL, BAD_CAST "missionInfo", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "mission", BAD_CAST info->mission);
  xmlNewChild(parent, NULL, BAD_CAST "orbitPhase", iStr(info->orbitPhase));
  xmlNewChild(parent, NULL, BAD_CAST "orbitCycle", iStr(info->orbitCycle));
  xmlNewChild(parent, NULL, BAD_CAST "absOrbit", iStr(info->absOrbit));
  xmlNewChild(parent, NULL, BAD_CAST "relOrbit", iStr(info->relOrbit));
  xmlNewChild(parent, NULL, BAD_CAST "numOrbitsInCycle", 
	      iStr(info->numOrbitsInCycle));
  if (info->orbitDirection == ASCENDING)
    strcpy(str, "ASCENDING");
  else if (info->orbitDirection == DESCENDING)
    strcpy(str, "DESCENDING");
  else if (info->orbitDirection == UNDEF_ORBIT)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "orbitDirection", BAD_CAST str);
  
  parent = xmlNewChild(section, NULL, BAD_CAST "acquisitionInfo", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "sensor", BAD_CAST info->sensor);
  if (info->imageMode == FINE_BEAM)
    strcpy(str, "FINE BEAM");
  else if (info->imageMode == STANDARD_BEAM)
    strcpy(str, "STANDARD BEAM");
  else if (info->imageMode == STRIPMAP_IMAGE)
    strcpy(str, "STRIPMAP");
  else if (info->imageMode == SCANSAR_IMAGE)
    strcpy(str, "SCANSAR");
  else if (info->imageMode == SPOTLIGHT_IMAGE)
    strcpy(str, "SPOTLIGHT");
  else if (info->imageMode == UNDEF_IMAGE_MODE)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "imageMode", BAD_CAST str);
  if (info->lookDirection == RIGHT_LOOK)
    strcpy(str, "RIGHT");
  else if (info->lookDirection == LEFT_LOOK)
    strcpy(str, "LEFT");
  else if (info->lookDirection == UNDEF_LOOK)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "lookDirection", BAD_CAST str);
  if (info->polarizationMode == SINGLE_POL)
    strcpy(str, "SINGLE");
  else if (info->polarizationMode == DUAL_POL)
    strcpy(str, "DUAL");
  else if (info->polarizationMode == QUAD_POL)
    strcpy(str, "QUAD");
  else if (info->polarizationMode == UNDEF_POL_MODE)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "polarizationMode", BAD_CAST str);
  node = xmlNewChild(parent, NULL, BAD_CAST "polarizationList", NULL);
  for (ii=0; ii<comps->numLayers; ii++) {
    if (info->polLayer[ii] == HH_POL)
      strcpy(str, "HH");
    else if (info->polLayer[ii] == HV_POL)
      strcpy(str, "HV");
    else if (info->polLayer[ii] == VH_POL)
      strcpy(str, "VH");
    else if (info->polLayer[ii] == VV_POL)
      strcpy(str, "VV");
    else if (info->polLayer[ii] == UNDEF_POL_LAYER)
      strcpy(str, "UNDEFINED");
    xmlNewChild(node, NULL, BAD_CAST "polLayer", BAD_CAST str);
  }
  xmlNewChild(parent, NULL, BAD_CAST "elevationBeamConfiguration", 
	      BAD_CAST info->elevationBeamConfiguration);
  node = xmlNewChild(parent, NULL, BAD_CAST "imagingModeSpecificInfo", NULL);
  
  info->imageMode = STANDARD_BEAM;
  
  if (info->imageMode == FINE_BEAM ||
      info->imageMode == STANDARD_BEAM ||
      info->imageMode == STRIPMAP_IMAGE) {
    if (info->imageMode == FINE_BEAM)
      parent = xmlNewChild(node, NULL, BAD_CAST "fineBeam", NULL);
    else if (info->imageMode == STANDARD_BEAM)
      parent = xmlNewChild(node, NULL, BAD_CAST "standardBeam", NULL);
    else if (info->imageMode == STRIPMAP_IMAGE)
      parent = xmlNewChild(node, NULL, BAD_CAST "stripMap", NULL);
    xmlNewChild(parent, NULL, BAD_CAST "azimuthBeamID", 
		BAD_CAST info->azimuthBeamID);
  }
  else if (info->imageMode == SCANSAR_IMAGE) {
    parent = xmlNewChild(node, NULL, BAD_CAST "scanSAR", NULL);
    xmlNewChild(parent, NULL, BAD_CAST "numberOfBeams", 
		iStr(info->numberOfBeams));
    xmlNodePtr beams = xmlNewChild(parent, NULL, BAD_CAST "beamList", NULL);
    for (ii=0; ii<info->numberOfBeams; ii++)
      xmlNewChild(beams, NULL, BAD_CAST "beamID", BAD_CAST info->beamID[ii]);
    xmlNewChild(parent, NULL, BAD_CAST "azimuthBeamID", 
		BAD_CAST info->azimuthBeamID);
    xmlNewChild(parent, NULL, BAD_CAST "numberOfBursts", 
		iStr(info->numberOfBursts));
  }
  else if (info->imageMode == SPOTLIGHT_IMAGE) {
    parent = xmlNewChild(node, NULL, BAD_CAST "spotLight", NULL);
    xmlNewChild(parent, NULL, BAD_CAST "numberOfAzimuthBeams", 
		iStr(info->numberOfAzimuthBeams));
    xmlNewChild(parent, NULL, BAD_CAST "azimuthBeamIDFirst", 
		BAD_CAST info->azimuthBeamIDFirst);
    xmlNewChild(parent, NULL, BAD_CAST "azimuthBeamIDLast", 
		BAD_CAST info->azimuthBeamIDLast);
    unit = xmlNewChild(parent, NULL, BAD_CAST "azimuthSteeringAngleFirst",
		       fStr(info->azimuthSteeringAngleFirst));
    xmlNewProp(node, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(parent, NULL, BAD_CAST "azimuthSteeringAngleLast",
		       fStr(info->azimuthSteeringAngleLast));
    xmlNewProp(node, BAD_CAST "units", BAD_CAST "degrees");
  }
  
  parent = xmlNewChild(section, NULL, BAD_CAST "productVariantInfo", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "productType", BAD_CAST info->productType);
  if (info->productVariant == SLC_PRODUCT)
    strcpy(str, "SLC");
  else if (info->productVariant == STD_PRODUCT)
    strcpy(str, "STD");
  else if (info->productVariant == TC_PRODUCT)
    strcpy(str, "TC");
  else if (info->productVariant == RTC_PRODUCT)
    strcpy(str, "RTC");
  else if (info->productVariant == GEO_PRODUCT)
    strcpy(str, "GEO");
  else if (info->productVariant == SSC_PRODUCT)
    strcpy(str, "SSC");
  else if (info->productVariant == MGD_PRODUCT)
    strcpy(str, "MGD");
  else if (info->productVariant == GEC_PRODUCT)
    strcpy(str, "GEC");
  else if (info->productVariant == EEC_PRODUCT)
    strcpy(str, "EEC");
  else if (info->productVariant == UNDEF_PRODUCT)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "productVariant", BAD_CAST str);
  if (info->projection == SLANTRANGE_PROJ)
    strcpy(str, "SLANTRANGE");
  else if (info->projection == GROUNDRANGE_PROJ)
    strcpy(str, "GROUNDRANGE");
  else if (info->projection == MAP_PROJ)
    strcpy(str, "MAP");
  else if (info->projection == UNDEF_PROJ)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "projection", BAD_CAST str);
  if (info->projection == MAP_PROJ) {
    if (info->mapProjection == UTM_PROJ)
      strcpy(str, "UTM");
    else if (info->mapProjection == PS_PROJ)
      strcpy(str, "POLARSTEREOGRAPHIC");
    else if (info->mapProjection == GEOG_PROJ)
      strcpy(str, "GEOGRAPHIC");
    else if (info->mapProjection == UNDEF_MAP)
      strcpy(str, "UNDEFINED");
    xmlNewChild(parent, NULL, BAD_CAST "mapProjection", BAD_CAST str);
  }
  if (info->resolutionVariant == SE_RES)
    strcpy(str, "SE");
  else if (info->resolutionVariant == RE_RES)
    strcpy(str, "RE");
  else if (info->resolutionVariant == UNDEF_RES)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "resolutionVariant", BAD_CAST str);
  if (info->radiometricCorrection == CALIBRATED)
    strcpy(str, "CALIBRATED");
  else if (info->radiometricCorrection == RELCALIBRATED)
    strcpy(str, "RELATIVE CALIBRATED");
  else if (info->radiometricCorrection == NOTCALIBRATED)
    strcpy(str, "NOT CALIBRATED");
  else if (info->radiometricCorrection == UNDEF_CAL)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "radiometricCorrection", BAD_CAST str);
  
  parent = xmlNewChild(section, NULL, BAD_CAST "imageDataInfo", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "pixelValueID", 
	      BAD_CAST info->pixelValueID);
  if (info->imageDataType == DETECTED_DATA_TYPE)
    strcpy(str, "DETECTED");
  else if (info->imageDataType == COMPLEX_DATA_TYPE)
    strcpy(str, "COMPLEX");
  else if (info->imageDataType == RAW_DATA_TYPE)
    strcpy(str, "RAW");
  else if (info->imageDataType == UNDEF_DATA_TYPE)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "imageDataType", BAD_CAST str);
  if (info->imageDataFormat == CEOS_DATA_FORMAT)
    strcpy(str, "CEOS");
  else if (info->imageDataFormat == GEOTIFF_DATA_FORMAT)
    strcpy(str, "GEOTIFF");
  else if (info->imageDataFormat == HDF5_DATA_FORMAT)
    strcpy(str, "HDF5");
  else if (info->imageDataFormat == UNDEF_DATA_FORMAT)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "imageDataFormat", BAD_CAST str);
  xmlNewChild(parent, NULL, BAD_CAST "numberOfLayers", 
	      iStr(info->numberOfLayers));
  xmlNewChild(parent, NULL, BAD_CAST "imageDataDepth", 
	      iStr(info->imageDataDepth));
  if (info->imageStorageOrder == ROWBYROW)
    strcpy(str, "ROWBYROW");
  else if (info->imageStorageOrder == COLBYCOL)
    strcpy(str, "COLBYCOL");
  else if (info->imageStorageOrder == UNDEF_STORE)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "imageStoreOrder", BAD_CAST str);
  xmlNewChild(parent, NULL, BAD_CAST "rowContent", BAD_CAST info->rowContent);
  xmlNewChild(parent, NULL, BAD_CAST "columnContent", 
	      BAD_CAST info->columnContent);
  node = xmlNewChild(parent, NULL, BAD_CAST "imageRaster", NULL);
  xmlNewChild(node, NULL, BAD_CAST "numberOfRows", iStr(info->numberOfRows));
  xmlNewChild(node, NULL, BAD_CAST "numberOfColumns", 
	      iStr(info->numberOfColumns));
  unit = xmlNewChild(node, NULL, BAD_CAST "rowSpacing", 
		     fStr(info->rowSpacing));
  if (info->projection == MAP_PROJ)
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");  
  else
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  unit = xmlNewChild(node, NULL, BAD_CAST "columnSpacing", 
		     fStr(info->columnSpacing));
  if (info->projection == MAP_PROJ)
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");  
  else
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  unit = xmlNewChild(node, NULL, BAD_CAST "groundRangeResolution", 
		     lfStr(info->groundRangeResolution));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  unit = xmlNewChild(node, NULL, BAD_CAST "azimuthResolution",
		     lfStr(info->azimuthResolution));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  xmlNewChild(node, NULL, BAD_CAST "azimuthLooks", fStr(info->azimuthLooks));
  xmlNewChild(node, NULL, BAD_CAST "rangeLooks", fStr(info->rangeLooks));
  
  parent = xmlNewChild(section, NULL, BAD_CAST "sceneInfo", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "sceneID",  BAD_CAST info->sceneID);
  node = xmlNewChild(parent, NULL, BAD_CAST "start", NULL);
  dateTime2str(info->startTimeUTC, str);
  xmlNewChild(node, NULL, BAD_CAST "timeUTC", BAD_CAST str);
  /*
    unit = xmlNewChild(node, NULL, BAD_CAST "timeGPS", ldStr(info->startTimeGPS));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
    unit = xmlNewChild(node, NULL, BAD_CAST "timeGPSFraction", 
    fStr(info->startTimeGPSFraction));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  */
  node = xmlNewChild(parent, NULL, BAD_CAST "stop", NULL);
  dateTime2str(info->stopTimeUTC, str);
  xmlNewChild(node, NULL, BAD_CAST "timeUTC", BAD_CAST str);
  /*
    unit = xmlNewChild(node, NULL, BAD_CAST "timeGPS", ldStr(info->stopTimeGPS));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
    unit = xmlNewChild(node, NULL, BAD_CAST "timeGPSFraction", 
    fStr(info->stopTimeGPSFraction));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  */
  node = xmlNewChild(parent, NULL, BAD_CAST "rangeTime", NULL);
  unit = xmlNewChild(node, NULL, BAD_CAST "firstPixel", 
		     lfStr(info->rangeTimeFirstPixel));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  unit = xmlNewChild(node, NULL, BAD_CAST "lastPixel", 
		     lfStr(info->rangeTimeLastPixel));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  unit = xmlNewChild(parent, NULL, BAD_CAST "sceneAzimuthExtent",
		     lf3Str(info->sceneAzimuthExtent));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  unit = xmlNewChild(parent, NULL, BAD_CAST "sceneRangeExtent", 
		     lf3Str(info->sceneRangeExtent));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  node = xmlNewChild(parent, NULL, BAD_CAST "sceneCenterCoord", NULL);
  if (info->sceneCenterCoord.refRow)
    xmlNewChild(node, NULL, BAD_CAST "refRow", 
		iStr(*info->sceneCenterCoord.refRow));
  if (info->sceneCenterCoord.refColumn)
    xmlNewChild(node, NULL, BAD_CAST "refColumn", 
		iStr(*info->sceneCenterCoord.refColumn));
  unit = xmlNewChild(node, NULL, BAD_CAST "lat", 
		     f5Str(info->sceneCenterCoord.lat));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  unit = xmlNewChild(node, NULL, BAD_CAST "lon", 
		     f5Str(info->sceneCenterCoord.lon));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  dateTime2str(info->sceneCenterCoord.azimuthTimeUTC, str);
  xmlNewChild(node, NULL, BAD_CAST "azimuthTimeUTC", BAD_CAST str);
  unit = xmlNewChild(node, NULL, BAD_CAST "rangeTime", 
		     lfStr(info->sceneCenterCoord.rangeTime));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  unit = xmlNewChild(node, NULL, BAD_CAST "incidenceAngle",
		     lf5Str(info->sceneCenterCoord.incidenceAngle));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  unit = xmlNewChild(parent, NULL, BAD_CAST "sceneAverageHeight",
		     lf3Str(info->sceneAverageHeight));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  for (ii=0; ii<4; ii++) {
    node = xmlNewChild(parent, NULL, BAD_CAST "sceneCornerCoord", NULL);
    if (info->sceneCornerCoord[ii].refRow)
      xmlNewChild(node, NULL, BAD_CAST "refRow", 
		  iStr(*info->sceneCornerCoord[ii].refRow));
    if (info->sceneCornerCoord[ii].refColumn)
      xmlNewChild(node, NULL, BAD_CAST "refColumn",
		  iStr(*info->sceneCornerCoord[ii].refColumn));
    unit = xmlNewChild(node, NULL, BAD_CAST "lat", 
		       f5Str(info->sceneCornerCoord[ii].lat));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node, NULL, BAD_CAST "lon", 
		       f5Str(info->sceneCornerCoord[ii].lon));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    dateTime2str(info->sceneCornerCoord[ii].azimuthTimeUTC, str);
    xmlNewChild(node, NULL, BAD_CAST "azimuthTimeUTC", BAD_CAST str);
    unit = xmlNewChild(node, NULL, BAD_CAST "rangeTime", 
		       lfStr(info->sceneCornerCoord[ii].rangeTime));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
    unit = xmlNewChild(node, NULL, BAD_CAST "incidenceAngle",
		       lf5Str(info->sceneCornerCoord[ii].incidenceAngle));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  }
  unit = xmlNewChild(parent, NULL, BAD_CAST "headingAngle", 
		     f5Str(info->headingAngle));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  
  parent = xmlNewChild(section, NULL, BAD_CAST "previewInfo", NULL);
  node = xmlNewChild(parent, NULL, BAD_CAST "quicklooks", NULL);
  xmlNewChild(node, NULL, BAD_CAST "imageDataFormat", 
	      BAD_CAST info->quicklooks.imageDataFormat);
  xmlNewChild(node, NULL, BAD_CAST "imageDataDepth", 
	      iStr(info->quicklooks.imageDataDepth));
  node2 = xmlNewChild(node, NULL, BAD_CAST "imageRaster", NULL);
  xmlNewChild(node2, NULL, BAD_CAST "numberOfRows", 
	      iStr(info->quicklooks.numberOfRows));
  xmlNewChild(node2, NULL, BAD_CAST "numberOfColumns", 
	      iStr(info->quicklooks.numberOfColumns));
  unit = xmlNewChild(node2, NULL, BAD_CAST "columnBlockLength",
		     fStr(info->quicklooks.columnBlockLength));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "pixels");
  unit = xmlNewChild(node2, NULL, BAD_CAST "rowBlockLength",
		     fStr(info->quicklooks.rowBlockLength));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "pixels");
  unit = xmlNewChild(node2, NULL, BAD_CAST "rowSpacing", 
		     fStr(info->quicklooks.rowSpacing));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  unit = xmlNewChild(node2, NULL, BAD_CAST "columnSpacing",
		     fStr(info->quicklooks.columnSpacing));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  /*
    node = xmlNewChild(parent, NULL, BAD_CAST "compositeQuicklook", NULL);
    xmlNewChild(node, NULL, BAD_CAST "imageDataFormat", 
    BAD_CAST info->compositeQLImageDataFormat);
    xmlNewChild(node, NULL, BAD_CAST "imageDataDepth", 
    iStr(info->compositeQLImageDataDepth));
    node2 = xmlNewChild(node, NULL, BAD_CAST "polLayerCode", NULL);
    
    info->compositeQLPolLayerCode = (iso_polColor *) MALLOC(sizeof(iso_polColor));
    strcpy(info->compositeQLPolLayerCode[0].polarization, "HH");
    strcpy(info->compositeQLPolLayerCode[0].color, "grey");
    
    for (ii=0; ii<info->numberOfLayers; ii++) {
    xmlNewChild(node2, NULL, BAD_CAST "polarization", 
    BAD_CAST info->compositeQLPolLayerCode[ii].polarization);
    xmlNewChild(node2, NULL, BAD_CAST "color",
    BAD_CAST info->compositeQLPolLayerCode[ii].color);
    }
  */
  node = xmlNewChild(parent, NULL, BAD_CAST "browseImage", NULL);
  xmlNewChild(node, NULL, BAD_CAST "imageDataFormat", 
	      BAD_CAST info->browseImageDataFormat);
  xmlNewChild(node, NULL, BAD_CAST "imageDataDepth", 
	      iStr(info->browseImageDataDepth));
  xmlNewChild(parent, NULL, BAD_CAST "mapPlotFormat", 
	      BAD_CAST info->mapPlotFormat);
  
  // Product Specific
  iso_productSpecific *spec = iso->productSpecific;
  section = xmlNewChild(root, NULL, BAD_CAST "productSpecific", NULL);
  parent = xmlNewChild(section, NULL, BAD_CAST "complexImageInfo", NULL);
  unit = xmlNewChild(parent, NULL, BAD_CAST "commonPRF", 
		     lfStr(spec->commonPRF));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  unit = xmlNewChild(parent, NULL, BAD_CAST "commonRSF", 
		     lfStr(spec->commonRSF));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  unit = xmlNewChild(parent, NULL, BAD_CAST "slantRangeResolution", 
		     lfStr(spec->slantRangeResolution));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  unit = xmlNewChild(parent, NULL, BAD_CAST "projectedSpacingAzimuth",
		     fStr(spec->projectedSpacingAzimuth));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  node = xmlNewChild(parent, NULL, BAD_CAST "projectedSpacingRange", NULL);
  unit = xmlNewChild(node, NULL, BAD_CAST "groundNear", 
		     f3Str(spec->projectedSpacingGroundNearRange));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  unit = xmlNewChild(node, NULL, BAD_CAST "groundFar",
		     fStr(spec->projectedSpacingGroundFarRange));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  unit = xmlNewChild(node, NULL, BAD_CAST "slantRange",
		     fStr(spec->projectedSpacingSlantRange));
  if (spec->imageCoordinateType == RAW_COORD)
    strcpy(str, "RAW");
  else if (spec->imageCoordinateType == ZERODOPPLER)
    strcpy(str, "ZERODOPPLER");
  else if (spec->imageCoordinateType == UNDEF_COORD)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "imageCoordinateType", BAD_CAST str);
  if (spec->imageDataStartWith == EARLYAZNEARRG)
    strcpy(str, "EARLYAZNEARRG");
  else if (spec->imageDataStartWith == EARLYAZFARRG)
    strcpy(str, "EARLYAZFARRG");
  else if (spec->imageDataStartWith == LATEAZNEARRG)
    strcpy(str, "LATEAZNEARRG");
  else if (spec->imageDataStartWith == LATEAZFARRG)
    strcpy(str, "LATEAZFARRG");
  else if (spec->imageDataStartWith == UNDEF_DATA_START)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "imageDataStartWith", BAD_CAST str);
  if (spec->quicklookDataStartWith == EARLYAZNEARRG)
    strcpy(str, "EARLYAZNEARRG");
  else if (spec->quicklookDataStartWith == EARLYAZFARRG)
    strcpy(str, "EARLYAZFARRG");
  else if (spec->quicklookDataStartWith == LATEAZNEARRG)
    strcpy(str, "LATEAZNEARRG");
  else if (spec->quicklookDataStartWith == LATEAZFARRG)
    strcpy(str, "LATEAZFARRG");
  else if (spec->quicklookDataStartWith == UNDEF_DATA_START)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "quicklookDataStartWith", BAD_CAST str);
  if (spec->geocodedImageInfoFlag) {
    parent = xmlNewChild(section, NULL, BAD_CAST "geocodedImageInfo", NULL);
    node = xmlNewChild(parent, NULL, BAD_CAST "mapProjection", NULL);
    xmlNewChild(node, NULL, BAD_CAST "geodeticDatumID", 
		BAD_CAST spec->geodeticDatumID);
    xmlNewChild(node, NULL, BAD_CAST "projectionID", 
		BAD_CAST spec->projectionID);
    xmlNewChild(node, NULL, BAD_CAST "zoneID", BAD_CAST spec->zoneID);
    node2 = xmlNewChild(node, NULL, BAD_CAST "projectionCenter", NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "latitude", 
		       f5Str(spec->projectionCenterLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "longitude",
		       f5Str(spec->projectionCenterLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    node2 = xmlNewChild(node, NULL, BAD_CAST "mapOrigin", NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "easting",  
		       f3Str(spec->mapOriginEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "northing", 
		       f3Str(spec->mapOriginNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    xmlNewChild(node, NULL, BAD_CAST "scaleFactor", fStr(spec->scaleFactor));
    node  = xmlNewChild(parent, NULL, BAD_CAST "geoParameter", NULL);
    node2 = xmlNewChild(node, NULL, BAD_CAST "pixelSpacing", NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "easting", 
		       fStr(spec->pixelSpacingEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "northing", 
		       fStr(spec->pixelSpacingNorthing));
    node2 = xmlNewChild(node, NULL, BAD_CAST "frameCoordsGeographic", NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftLatitude",
		       f5Str(spec->frameCoordsGeographic.upperLeftLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftLongitude",
		       f5Str(spec->frameCoordsGeographic.upperLeftLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightLatitude",
		       f5Str(spec->frameCoordsGeographic.upperRightLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightLongitude",
		       f5Str(spec->frameCoordsGeographic.upperRightLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftLatitude",
		       f5Str(spec->frameCoordsGeographic.lowerLeftLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftLongitude",
		       f5Str(spec->frameCoordsGeographic.lowerLeftLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightLatitude",
		       f5Str(spec->frameCoordsGeographic.lowerRightLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightLongitude",
		       f5Str(spec->frameCoordsGeographic.lowerRightLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    node2 = xmlNewChild(node, NULL, BAD_CAST "frameCoordsCartographic", NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftEasting",
		       f3Str(spec->frameCoordsCartographic.upperLeftEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftNorthing",
		       f3Str(spec->frameCoordsCartographic.upperLeftNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightEasting",
		       f3Str(spec->frameCoordsCartographic.upperRightEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightNorthing",
		       f3Str(spec->frameCoordsCartographic.upperRightNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightEasting",
		       f3Str(spec->frameCoordsCartographic.lowerRightEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightNorthing",
		       f3Str(spec->frameCoordsCartographic.lowerRightNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftEasting",
		       f3Str(spec->frameCoordsCartographic.lowerLeftEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftNorthing",
		       f3Str(spec->frameCoordsCartographic.lowerLeftNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCoordsGeographic", NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftLatitude",
		       f5Str(spec->sceneCoordsGeographic.upperLeftLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftLongitude",
		       f5Str(spec->sceneCoordsGeographic.upperLeftLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightLatitude",
		       f5Str(spec->sceneCoordsGeographic.upperRightLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightLongitude",
		       f5Str(spec->sceneCoordsGeographic.upperRightLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftLatitude",
		       f5Str(spec->sceneCoordsGeographic.lowerLeftLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftLongitude",
		       f5Str(spec->sceneCoordsGeographic.lowerLeftLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightLatitude",
		       f5Str(spec->sceneCoordsGeographic.lowerRightLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightLongitude",
		       f5Str(spec->sceneCoordsGeographic.lowerRightLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCoordsCartographic", NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftEasting",
		       f3Str(spec->sceneCoordsCartographic.upperLeftEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftNorthing",
		       f3Str(spec->sceneCoordsCartographic.upperLeftNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightEasting",
		       f3Str(spec->sceneCoordsCartographic.upperRightEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightNorthing",
		       f3Str(spec->sceneCoordsCartographic.upperRightNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightEasting",
		       f3Str(spec->sceneCoordsCartographic.lowerRightEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightNorthing",
		       f3Str(spec->sceneCoordsCartographic.lowerRightNorthing)); 
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftEasting",
		       f3Str(spec->sceneCoordsCartographic.lowerLeftEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftNorthing",
		       f3Str(spec->sceneCoordsCartographic.lowerLeftNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCenterCoordsGeographic", 
			NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "centerCoordLatitude",
		       f5Str(spec->sceneCenterCoordLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "centerCoordLongitude",
		       f5Str(spec->sceneCenterCoordLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCenterCoordsCartograhic", 
			NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "centerCoordEasting",
		       f3Str(spec->sceneCenterCoordEasting));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node2, NULL, BAD_CAST "centerCoordNorthing",
		       f3Str(spec->sceneCenterCoordNorthing));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    if (spec->imageResamplingMethod == NEAREST_NEIGHBOR_RESAMPLE)
      strcpy(str, "NEAREST_NEIGHBOR");
    else if (spec->imageResamplingMethod == BILINEAR_RESAMPLE)
      strcpy(str, "BILINEAR");
    else if (spec->imageResamplingMethod == CUBIC_CONVOLUTION_RESAMPLE)
      strcpy(str, "CUBIC_CONVOLUTION");
    else if (spec->imageResamplingMethod == UNDEF_RESAMPLE)
      strcpy(str, "UNDEFINED");
    xmlNewChild(node, NULL, BAD_CAST "imageResamplingMethod", BAD_CAST str);
    if (spec->elevationDataFlag) {
      node = xmlNewChild(parent, NULL, BAD_CAST "elevationData", NULL);
      xmlNewChild(node, NULL, BAD_CAST "dataSource", 
		  BAD_CAST spec->elevationDataSource);
      unit = xmlNewChild(node, NULL, BAD_CAST "minimumHeight", 
			 f3Str(spec->elevationMinimumHeight));
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
      unit = xmlNewChild(node, NULL, BAD_CAST "meanHeight",
			 f5Str(spec->elevationMeanHeight));
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
      unit = xmlNewChild(node, NULL, BAD_CAST "maximumHeight",
			 f3Str(spec->elevationMaximumHeight));
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    }
    if (spec->incidenceAngleMaskDescriptionFlag) {
      node = xmlNewChild(parent, NULL, BAD_CAST "incidenceAngleMaskDescription",
			 NULL);
      xmlNewChild(node, NULL, BAD_CAST "pixelValueID", 
		  BAD_CAST spec->incidenceAnglePixelValueID);
      if (spec->incidenceAngleImageDataFormat == CEOS_DATA_FORMAT)
	strcpy(str, "CEOS");
      else if (spec->incidenceAngleImageDataFormat == GEOTIFF_DATA_FORMAT)
	strcpy(str, "GEOTIFF");
      else if (spec->incidenceAngleImageDataFormat == HDF5_DATA_FORMAT)
	strcpy(str, "HDF5");
      else if (spec->incidenceAngleImageDataFormat == COSAR_DATA_FORMAT)
	strcpy(str, "COSAR");
      else if (spec->incidenceAngleImageDataFormat == UNDEF_DATA_FORMAT)
	strcpy(str, "UNDEFINED");
      xmlNewChild(node, NULL, BAD_CAST "imageDataFormat", BAD_CAST str);
      xmlNewChild(node, NULL, BAD_CAST "imageDataDepth", 
		  iStr(spec->incidenceAngleImageDataDepth));
      node2 = xmlNewChild(node, NULL, BAD_CAST "imageRaster", NULL);
      xmlNewChild(node2, NULL, BAD_CAST "numberOfRows",
		  iStr(spec->incidenceAngleNumberOfRows));
      xmlNewChild(node2, NULL, BAD_CAST "numberOfColumns",
		  iStr(spec->incidenceAngleNumberOfColumns));
      unit = xmlNewChild(node2, NULL, BAD_CAST "rowSpacing",
			 fStr(spec->incidenceAngleRowSpacing));
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
      unit = xmlNewChild(node2, NULL, BAD_CAST "columnSpacing",
			 fStr(spec->incidenceAngleColumnSpacing));
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    }
  }
  
  // Setup
  iso_setup *setup = iso->setup;
  section = xmlNewChild(root, NULL, BAD_CAST "setup", NULL);
  parent = xmlNewChild(section, NULL, BAD_CAST "orderInfo", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "orderType", BAD_CAST setup->orderType);
  xmlNewChild(parent, NULL, BAD_CAST "processingPriority", 
	      BAD_CAST setup->processingPriority);
  if (setup->orbitAccuracy == PREDICTED_ORBIT)
    strcpy(str, "PREDICTED");
  else if (setup->orbitAccuracy == RESTITUTED_ORBIT)
    strcpy(str, "RESTITUTED");
  else if (setup->orbitAccuracy == PRECISE_ORBIT)
    strcpy(str, "PRECISE");
  else if (setup->orbitAccuracy == TLE)
    strcpy(str, "TLE");
  else if (setup->orbitAccuracy == UNDEF_ORBIT_ACC)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "orbitAccuracy", BAD_CAST str);
  if (setup->sceneSpecification == FRAME_SPEC)
    strcpy(str, "FRAME");
  else if (setup->sceneSpecification == TIME_SPEC)
    strcpy(str, "TIME");
  else if (setup->sceneSpecification == CENTERCOORDS_SPEC)
    strcpy(str, "CENTERCOORDS");
  else if (setup->sceneSpecification == UNDEF_SCENE_SPEC)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "sceneSpecification", BAD_CAST str);
  node = xmlNewChild(parent, NULL, BAD_CAST "orderedScene", NULL);
  if (setup->sceneSpecification == FRAME_SPEC)
    xmlNewChild(node, NULL, BAD_CAST "frameID", iStr(setup->frameID));
  else if (setup->sceneSpecification == TIME_SPEC) {
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneExtent", NULL);
    dateTime2str(setup->sceneStartTimeUTC, str);
    xmlNewChild(node2, NULL, BAD_CAST "startTimeUTC", BAD_CAST str);
    dateTime2str(setup->sceneStopTimeUTC, str);
    xmlNewChild(node2, NULL, BAD_CAST "stopTimeUTC", BAD_CAST str);
  }
  else if (setup->sceneSpecification == CENTERCOORDS_SPEC) {
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCenterCoord", NULL);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lat", 
		       f5Str(setup->sceneCenterLatitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    unit = xmlNewChild(node2, NULL, BAD_CAST "lon", 
		       f5Str(setup->sceneCenterLongitude));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  }
  if (setup->imagingMode == FINE_BEAM)
    strcpy(str, "FINE_BEAM");
  else if (setup->imagingMode == STANDARD_BEAM)
    strcpy(str, "STANDARD_BEAM");
  else if (setup->imagingMode == STRIPMAP_IMAGE)
    strcpy(str, "STRIPMAP");
  else if (setup->imagingMode == SCANSAR_IMAGE)
    strcpy(str, "SCANSAR");
  else if (setup->imagingMode == SPOTLIGHT_IMAGE)
    strcpy(str, "SPOTLIGHT");
  else if (setup->imagingMode == UNDEF_IMAGE_MODE)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "imagingMode", BAD_CAST str);
  if (setup->lookDirection == RIGHT_LOOK)
    strcpy(str, "RIGHT");
  else if (setup->lookDirection == LEFT_LOOK)
    strcpy(str, "LEFT");
  else if (setup->lookDirection == UNDEF_LOOK)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "lookDirection", BAD_CAST str);
  if (setup->polarizationMode == SINGLE_POL)
    strcpy(str, "SINGLE");
  else if (setup->polarizationMode == DUAL_POL)
    strcpy(str, "DUAL");
  else if (setup->polarizationMode == QUAD_POL)
    strcpy(str, "QUAD");
  else if (setup->polarizationMode == UNDEF_POL_MODE)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "polarizationMode", BAD_CAST str);
  if (setup->polLayer == HH_POL)
    strcpy(str, "HH");
  else if (setup->polLayer == HV_POL)
    strcpy(str, "HV");
  else if (setup->polLayer == VH_POL)
    strcpy(str, "VH");
  else if (setup->polLayer == VV_POL)
    strcpy(str, "VV");
  else if (setup->polLayer == UNDEF_POL_LAYER)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "polLayer", BAD_CAST str);
  xmlNewChild(node, NULL, BAD_CAST "elevationBeamConfiguration", 
	      BAD_CAST setup->elevationBeamConfiguration);
  if (setup->productVariant == SLC_PRODUCT)
    strcpy(str, "SLC");
  else if (setup->productVariant == STD_PRODUCT)
    strcpy(str, "STD");
  else if (setup->productVariant == TC_PRODUCT)
    strcpy(str, "TC");
  else if (setup->productVariant == RTC_PRODUCT)
    strcpy(str, "RTC");
  else if (setup->productVariant == GEO_PRODUCT)
    strcpy(str, "GEO");
  else if (setup->productVariant == SSC_PRODUCT)
    strcpy(str, "SSC");
  else if (setup->productVariant == MGD_PRODUCT)
    strcpy(str, "MGD");
  else if (setup->productVariant == GEC_PRODUCT)
    strcpy(str, "GEC");
  else if (setup->productVariant == EEC_PRODUCT)
    strcpy(str, "EEC");
  else if (setup->productVariant == UNDEF_PRODUCT)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "productVariant", BAD_CAST str);
  if (setup->resolutionVariant == SE_RES)
    strcpy(str, "SE");
  else if (setup->resolutionVariant == RE_RES)
    strcpy(str, "RE");
  else if (setup->resolutionVariant == UNDEF_RES)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "resolutionVariant", BAD_CAST str);
  if (setup->projection == SLANTRANGE_PROJ)
    strcpy(str, "SLANTRANGE");
  else if (setup->projection == GROUNDRANGE_PROJ)
    strcpy(str, "GROUNDRANGE");
  else if (setup->projection == MAP_PROJ)
    strcpy(str, "MAP");
  else if (setup->projection == UNDEF_PROJ)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "projection", BAD_CAST str);
  parent = xmlNewChild(section, NULL, BAD_CAST "inputData", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "logicalDataTakeID", 
	      BAD_CAST setup->logicalDataTakeID);
  xmlNewChild(parent, NULL, BAD_CAST "level0ProductID", 
	      BAD_CAST setup->level0ProductID);
  dateTime2str(setup->L0SARGenerationTimeUTC, str);
  xmlNewChild(parent, NULL, BAD_CAST "L0SARGenerationTimeUTC", BAD_CAST str);
  if (setup->numProcessingSteps > 0) {
    parent = xmlNewChild(section, NULL, BAD_CAST "processingSteps", NULL);
    char step[5];
    for (ii=0; ii<setup->numProcessingSteps; ii++) {
      sprintf(step, "%ld", ii+1);
      node = xmlNewChild(parent, NULL, BAD_CAST "software", NULL);
      xmlNewProp(node, BAD_CAST "step", BAD_CAST step);
      xmlNewChild(node, NULL, BAD_CAST "softwareID", 
		  BAD_CAST setup->processingStep[ii].softwareID);
      xmlNewChild(node, NULL, BAD_CAST "softwareVersion",
		  BAD_CAST setup->processingStep[ii].softwareVersion);
      if (setup->processingStep[ii].algorithm)
	xmlNewChild(node, NULL, BAD_CAST "algorithm", 
		    BAD_CAST setup->processingStep[ii].algorithm);
    }
  }
  
  // Processing
  iso_processing *proc = iso->processing;
  section = xmlNewChild(root, NULL, BAD_CAST "processing", NULL);
  parent = xmlNewChild(section, NULL, BAD_CAST "doppler", NULL);
  xmlNewChild(parent, NULL, BAD_CAST "dopplerBasebandEstimationMethod",
	      BAD_CAST proc->dopplerBasebandEstimationMethod);
  if (proc->dopplerCentroidCoordinateType == RAW_COORD)
    strcpy(str, "RAW");
  else if (proc->dopplerCentroidCoordinateType == ZERODOPPLER)
    strcpy(str, "ZERODOPPLER");
  else if (proc->dopplerCentroidCoordinateType == UNDEF_COORD)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "dopplerCentroidCoordinateType", 
	      BAD_CAST str);
  if (proc->doppler) {
    for (ii=0; ii<comps->numLayers; ii++) {
      node = xmlNewChild(parent, NULL, BAD_CAST "dopplerCentroid", NULL);
      xmlNewProp(node, BAD_CAST "layerIndex", ldStr(ii+1));
      if (proc->doppler[ii].polLayer == HH_POL)
	strcpy(str, "HH");
      else if (proc->doppler[ii].polLayer == HV_POL)
	strcpy(str, "HV");
      else if (proc->doppler[ii].polLayer == VH_POL)
	strcpy(str, "VH");
      else if (proc->doppler[ii].polLayer == VV_POL)
	strcpy(str, "VV");
      else if (proc->doppler[ii].polLayer == UNDEF_POL_LAYER)
	strcpy(str, "UNDEFINED");
      xmlNewChild(node, NULL, BAD_CAST "polLayer", BAD_CAST str);
      xmlNewChild(node, NULL, BAD_CAST "numberOfBlocks",
		  iStr(proc->doppler[ii].numberOfBlocks));
      xmlNewChild(node, NULL, BAD_CAST "numberOfRejectedBlocks",
		  iStr(proc->doppler[ii].numberOfRejectedBlocks));
      xmlNewChild(node, NULL, BAD_CAST "numberOfDopplerRecords",
		  iStr(proc->doppler[ii].numberOfDopperRecords));
      node2 = xmlNewChild(node, NULL, BAD_CAST "dopplerEstimate", NULL);
      dateTime2str(proc->doppler[ii].timeUTC, str);
      xmlNewChild(node2, NULL, BAD_CAST "timeUTC", BAD_CAST str);
      xmlNewChild(node2, NULL, BAD_CAST "dopplerAtMidRange",
		  lfStr(proc->doppler[ii].dopplerAtMidRange));
      int degree = proc->doppler[ii].polynomialDegree;
      xmlNewChild(node2, NULL, BAD_CAST "polynomialDegree", iStr(degree));
      node3 = xmlNewChild(node2, NULL, BAD_CAST "basebandDoppler", NULL);
      for (kk=0; kk<=degree; kk++) {
	unit = xmlNewChild(node3, NULL, BAD_CAST "coefficient", 
			   lfStr(proc->doppler[ii].coefficient[kk]));
	xmlNewProp(unit, "exponent", ldStr(kk));
      }
    }
  }
  parent = xmlNewChild(section, NULL, BAD_CAST "processingParameter", NULL);
  if (proc->processingParameter[0].processingInfoCoordinateType == RAW_COORD)
    strcpy(str, "RAW");
  else if (proc->processingParameter[0].processingInfoCoordinateType == ZERODOPPLER)
    strcpy(str, "ZERODOPPLER");
  else if (proc->processingParameter[0].processingInfoCoordinateType == UNDEF_COORD)
    strcpy(str, "UNDEFINED");
  xmlNewChild(parent, NULL, BAD_CAST "processingInfoCoordinateType", 
	      BAD_CAST str);
  xmlNewChild(parent, NULL, BAD_CAST "rangeLooks",
	      fStr(proc->processingParameter[0].rangeLooks));
  xmlNewChild(parent, NULL, BAD_CAST "azimuthLooks",
	      fStr(proc->processingParameter[0].azimuthLooks));
  unit = xmlNewChild(parent, NULL, BAD_CAST "rangeLookBandwidth",
		     lfStr(proc->processingParameter[0].rangeLookBandwidth));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  unit = xmlNewChild(parent, NULL, BAD_CAST "azimuthLookBandwidth",
		     lfStr(proc->processingParameter[0].azimuthLookBandwidth));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  unit = xmlNewChild(parent, NULL, BAD_CAST "totalProcessedRangeBandwidth",
		     lfStr(proc->processingParameter[0].totalProcessedRangeBandwidth));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  unit = xmlNewChild(parent, NULL, BAD_CAST "totalProcessedAzimuthBandwidth",
		     lfStr(proc->processingParameter[0].totalProcessedAzimuthBandwidth));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  // rangeCompression ???

  parent = xmlNewChild(section, NULL, BAD_CAST "processingFlags", NULL);  
  xmlNewChild(parent, NULL, BAD_CAST "chirpReplicaUsedFlag",
	      bStr(proc->chirpReplicaUsedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "geometricDopplerUsedFlag",
	      bStr(proc->geometricDopplerUsedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "azimuthPatternCorrectedFlag",
	      bStr(proc->azimuthPatternCorrectedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "elevationPatternCorrectedFlag",
	      bStr(proc->elevationPatternCorrectedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "detectedFlag", bStr(proc->detectedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "multiLookedFlag", 
	      bStr(proc->multiLookedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "polarimetricProcessedFlag",
	      bStr(proc->polarimetricProcessedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "terrainCorrectedFlag",
	      bStr(proc->terrainCorrectedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "layoverShadowMaskGeneratedFlag",
	      bStr(proc->layoverShadowMaskGeneratedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "geocodedFlag", bStr(proc->geocodedFlag));
  xmlNewChild(parent, NULL, BAD_CAST "nominalProcessingPerformedFlag",
	      bStr(proc->nominalProcessingPerformedFlag));

  // Instrument
  iso_instrument *inst = iso->instrument;
  section = xmlNewChild(root, NULL, BAD_CAST "instrument", NULL);
  if (inst->instrumentInfoCoordinateType == RAW_COORD)
    strcpy(str, "RAW");
  else if (inst->instrumentInfoCoordinateType == ZERODOPPLER)
    strcpy(str, "ZERODOPPLER");
  else if (inst->instrumentInfoCoordinateType == UNDEF_COORD)
    strcpy(str, "UNDEFINED");
  xmlNewChild(section, NULL, BAD_CAST "instrumentInfoCoordinateType", 
	      BAD_CAST str);
  parent = xmlNewChild(section, NULL, BAD_CAST "radarParameters", NULL);
  unit = xmlNewChild(parent, NULL, BAD_CAST "centerFrequency",
		     lfStr(inst->centerFrequency));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  parent = xmlNewChild(section, NULL, BAD_CAST "settings", NULL);
  if (inst->settings) {
    for (ii=0; ii<comps->numLayers; ii++) {
      if (inst->settings[ii].polLayer == HH_POL)
	strcpy(str, "HH");
      else if (inst->settings[ii].polLayer == HV_POL)
	strcpy(str, "HV");
      else if (inst->settings[ii].polLayer == VH_POL)
	strcpy(str, "VH");
      else if (inst->settings[ii].polLayer == VV_POL)
	strcpy(str, "VV");
      else if (inst->settings[ii].polLayer == UNDEF_POL_LAYER)
	strcpy(str, "UNDEFINED");
      xmlNewChild(parent, NULL, BAD_CAST "polLayer", BAD_CAST str);
      xmlNewChild(parent, NULL, BAD_CAST "beamID", 
		  BAD_CAST inst->settings[ii].beamID);
      unit = xmlNewChild(parent, NULL, BAD_CAST "rxBandwidth",
			 lfStr(inst->settings[ii].rxBandwidth));
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
      unit = xmlNewChild(parent, NULL, BAD_CAST "RSF", 
		       lfStr(inst->settings[ii].rsf));
      
      inst->settings[ii].numberOfPRFChanges = 0;
      inst->settings[ii].numberOfEchoWindowPositionChanges = 0;
      inst->settings[ii].numberOfEchoWindowLengthChanges = 0;
      inst->settings[ii].numberOfSettingRecords = 1;
      
      xmlNewChild(parent, NULL, BAD_CAST "numberOfPRFChanges",
		  iStr(inst->settings[ii].numberOfPRFChanges));
      xmlNewChild(parent, NULL, BAD_CAST "numberOfEchoWindowPositionChanges",
		  iStr(inst->settings[ii].numberOfEchoWindowPositionChanges));
      xmlNewChild(parent, NULL, BAD_CAST "numberOfEchoWindowLengthChanges",
		  iStr(inst->settings[ii].numberOfEchoWindowLengthChanges));
      xmlNewChild(parent, NULL, BAD_CAST "numberOfSettingRecords",
		  iStr(inst->settings[ii].numberOfSettingRecords));
      node = xmlNewChild(parent, NULL, BAD_CAST "settingRecords", NULL);
      iso_settingRecord *rec = inst->settings[ii].settingRecord;
      for (kk=0; kk<inst->settings[ii].numberOfSettingRecords; kk++) {
	node2 = xmlNewChild(node, NULL, BAD_CAST "dataSegment", NULL);
	xmlNewProp(node2, BAD_CAST "segmentID", ldStr(kk+1));
	dateTime2str(rec[kk].startTimeUTC, str);
	xmlNewChild(node2, NULL, BAD_CAST "startTimeUTC", BAD_CAST str);
	dateTime2str(rec[kk].stopTimeUTC, str);
	xmlNewChild(node2, NULL, BAD_CAST "stopTimeUTC", BAD_CAST str);
	xmlNewChild(node2, NULL, BAD_CAST "numberOfRows", 
		    iStr(rec[kk].numberOfRows));
	unit = xmlNewChild(node, NULL, BAD_CAST "PRF", lfStr(rec[kk].prf));
	xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
	xmlNewChild(node, NULL, BAD_CAST "echoWindowPosition", 
		    lfStr(rec[kk].echoWindowPosition));
	xmlNewChild(node, NULL, BAD_CAST "echoWindowLength",
		    lfStr(rec[kk].echoWindowLength));
	xmlNewChild(node, NULL, BAD_CAST "pulseType", 
		    BAD_CAST rec[kk].pulseType);
      }
    }
  }
  
  // Calibration
  // section = xmlNewChild(root, NULL, "calibration", NULL);
  
  // Platform
  iso_platform *platform = iso->platform;
  section = xmlNewChild(root, NULL, BAD_CAST "platform", NULL);
  parent = xmlNewChild(section, NULL, BAD_CAST "orbit", NULL);
  node = xmlNewChild(parent, NULL, BAD_CAST "orbitHeader", NULL);
  if (platform->sensor == PREDICTED_SENSOR)
    strcpy(str, "PREDICTED");
  else if (platform->sensor == SINGLE_GPS)
    strcpy(str, "SINGLE GPS");
  else if (platform->sensor == DIFFERENTIAL_GPS)
    strcpy(str, "DIFFERENTIAL GPS");
  else if (platform->sensor == UNDEF_ORBIT_SENSOR)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "sensor", BAD_CAST str);
  if (platform->accuracy == PREDICTED_ORBIT)
    strcpy(str, "PREDICTED");
  else if (platform->accuracy == RESTITUTED_ORBIT)
    strcpy(str, "RESTITUTED");
  else if (platform->accuracy == PRECISE_ORBIT)
    strcpy(str, "PRECISE");
  else if (platform->accuracy == TLE)
    strcpy(str, "TLE");
  else if (platform->accuracy == UNDEF_ORBIT_ACC)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node, NULL, BAD_CAST "accuracy", BAD_CAST str);

  xmlNewChild(node, NULL, BAD_CAST "numStateVectors", 
	      iStr(platform->numStateVectors));
  dateTime2str(platform->firstStateTimeUTC, str);
  xmlNewChild(node, NULL, BAD_CAST "firstStateTimeUTC", BAD_CAST str);
  dateTime2str(platform->lastStateTimeUTC, str);
  xmlNewChild(node, NULL, BAD_CAST "lastStateTimeUTC", BAD_CAST str);
  xmlNewChild(node, NULL, BAD_CAST "stateVectorRefTime",
	      BAD_CAST platform->stateVectorRefFrame);
  unit = xmlNewChild(node, NULL, BAD_CAST "stateVectorTimeSpacing",
		     lfStr(platform->stateVectorTimeSpacing));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  char num[5];
  for (ii=0; ii<platform->numStateVectors; ii++) {
    node = xmlNewChild(parent, NULL, BAD_CAST "stateVec", NULL);
    sprintf(num, "%ld", ii+1); 
    xmlNewProp(node, BAD_CAST "num", BAD_CAST num);
    dateTime2str(platform->stateVec[ii].timeUTC, str);
    xmlNewChild(node, NULL, BAD_CAST "timeUTC", BAD_CAST str);
    unit = xmlNewChild(node, NULL, BAD_CAST "posX", 
		       lf3Str(platform->stateVec[ii].posX));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node, NULL, BAD_CAST "posY", 
		       lf3Str(platform->stateVec[ii].posY));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node, NULL, BAD_CAST "posZ", 
		       lf3Str(platform->stateVec[ii].posZ));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    unit = xmlNewChild(node, NULL, BAD_CAST "velX", 
		       lf3Str(platform->stateVec[ii].velX));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m/s");
    unit = xmlNewChild(node, NULL, BAD_CAST "velY", 
		       lf3Str(platform->stateVec[ii].velY));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m/s");
    unit = xmlNewChild(node, NULL, BAD_CAST "velZ", 
		       lf3Str(platform->stateVec[ii].velZ));
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m/s");
  }

  // Product Quality
  iso_productQuality *quality = iso->productQuality;
  section = xmlNewChild(root, NULL, BAD_CAST "productQuality", NULL);
  int numGaps;
  parent = xmlNewChild(section, NULL, BAD_CAST "rawDataQuality", NULL);
  if (quality->rawDataQuality) {
    for (ii=0; ii<info->numberOfLayers; ii++) {
      if (quality->rawDataQuality[ii].polLayer == HH_POL)
	strcpy(str, "HH");
      else if (quality->rawDataQuality[ii].polLayer == HV_POL)
	strcpy(str, "HV");
      else if (quality->rawDataQuality[ii].polLayer == VH_POL)
	strcpy(str, "VH");
      else if (quality->rawDataQuality[ii].polLayer == VV_POL)
	strcpy(str, "VV");
      else if (quality->rawDataQuality[ii].polLayer == UNDEF_POL_LAYER)
	strcpy(str, "UNDEFINED");    
      xmlNewChild(parent, NULL, BAD_CAST "polLayer", BAD_CAST str);
      if (quality->rawDataQuality[ii].beamID)
	xmlNewChild(parent, NULL, BAD_CAST "beamID", 
		    BAD_CAST quality->rawDataQuality[ii].beamID);
      numGaps = quality->rawDataQuality[ii].numGaps;
      xmlNewChild(parent, NULL, BAD_CAST "numGaps", iStr(numGaps));
      for (kk=0; kk<numGaps; kk++) {
	node = xmlNewChild(parent, NULL, BAD_CAST "gap", NULL);
	sprintf(num, "%ld", kk+1);
	xmlNewProp(node, BAD_CAST "num", BAD_CAST num);
	xmlNewChild(node, NULL, BAD_CAST "start", 
		    ldStr(quality->rawDataQuality[ii].gap[kk].start));
	xmlNewChild(node, NULL, BAD_CAST "length",
		    iStr(quality->rawDataQuality[ii].gap[kk].length));
	if (quality->rawDataQuality[ii].gap[kk].fill == RANDOM_FILL)
	  strcpy(str, "RANDOM");
	else if (quality->rawDataQuality[ii].gap[kk].fill == ZERO_FILL)
	  strcpy(str, "ZERO");
	else if (quality->rawDataQuality[ii].gap[kk].fill == UNDEF_FILL)
	  strcpy(str, "UNDEFINED");
	xmlNewChild(node, NULL, BAD_CAST "fill", BAD_CAST str);
      }
      xmlNewChild(parent, NULL, BAD_CAST "gapSignificanceFlag",
		  bStr(quality->rawDataQuality[ii].gapSignificanceFlag));
      xmlNewChild(parent, NULL, BAD_CAST "missingLinesSignificanceFlag",
		  bStr(quality->rawDataQuality[ii].missingLinesSignificanceFlag));
      xmlNewChild(parent, NULL, BAD_CAST "bitErrorSignificanceFlag",
		  bStr(quality->rawDataQuality[ii].bitErrorSignificanceFlag));
      xmlNewChild(parent, NULL, BAD_CAST "timeReconstructionSignificanceFlag",
		  bStr(quality->rawDataQuality[ii].timeReconstructionSignificanceFlag));    
    }
  }
  parent = xmlNewChild(section, NULL, BAD_CAST "processingParameterQuality", 
		       NULL);
  xmlNewChild(parent, NULL, BAD_CAST "dopplerAmbiguityNotZeroFlag",
	      bStr(quality->dopplerAmbiguityNotZeroFlag));
  xmlNewChild(parent, NULL, BAD_CAST "dopplerOutsideLimitsFlag",
	      bStr(quality->dopplerOutsideLimitsFlag));
  xmlNewChild(parent, NULL, BAD_CAST "geolocationQualityLowFlag",
	      bStr(quality->geolocationQualityLowFlag));
  if (quality->imageDataQuality) {
    for (ii=0; ii<info->numberOfLayers; ii++) {
      parent = xmlNewChild(section, NULL, BAD_CAST "imageDataQuality", NULL);
      sprintf(num, "%ld", ii+1);
      xmlNewProp(parent, BAD_CAST "layerIndex", BAD_CAST num);
      if (quality->imageDataQuality[ii].polLayer == HH_POL)
	strcpy(str, "HH");
      else if (quality->imageDataQuality[ii].polLayer == HV_POL)
	strcpy(str, "HV");
      else if (quality->imageDataQuality[ii].polLayer == VH_POL)
	strcpy(str, "VH");
      else if (quality->imageDataQuality[ii].polLayer == VV_POL)
	strcpy(str, "VV");
      else if (quality->imageDataQuality[ii].polLayer == UNDEF_POL_LAYER)
	strcpy(str, "UNDEFINED");    
      xmlNewChild(parent, NULL, BAD_CAST "polLayer", BAD_CAST str);
      if (quality->imageDataQuality[ii].beamID)
	xmlNewChild(parent, NULL, BAD_CAST "beamID", 
		    BAD_CAST quality->imageDataQuality[ii].beamID);
      node = xmlNewChild(parent, NULL, BAD_CAST "imageDataStatistics", NULL);
      xmlNewChild(node, NULL, BAD_CAST "min", 
		  lfStr(quality->imageDataQuality[ii].min));
      xmlNewChild(node, NULL, BAD_CAST "max", 
		  lfStr(quality->imageDataQuality[ii].max));
      xmlNewChild(node, NULL, BAD_CAST "mean", 
		  lfStr(quality->imageDataQuality[ii].mean));
      xmlNewChild(node, NULL, BAD_CAST "standardDeviation", 
		  lfStr(quality->imageDataQuality[ii].stdDev));
    }
  }
  parent = xmlNewChild(section, NULL, BAD_CAST "limits", NULL);
  node = xmlNewChild(parent, NULL, BAD_CAST "rawData", NULL);
  xmlNewChild(node, NULL, BAD_CAST "gapDefinition", 
	      iStr(quality->gapDefinition));
  xmlNewChild(node, NULL, BAD_CAST "gapPercentageLimit", 
	      fStr(quality->gapPercentageLimit));
  xmlNewChild(node, NULL, BAD_CAST "missingLinePercentageLimit",
	      fStr(quality->missingLinePercentageLimit));
  xmlNewChild(node, NULL, BAD_CAST "bitErrorLimit", 
	      fStr(quality->bitErrorLimit));
  xmlNewChild(node, NULL, BAD_CAST "timeReconstructionPercentageLimit",
	      fStr(quality->timeReconstructionPercentageLimit));
  node = xmlNewChild(parent, NULL, BAD_CAST "processing", NULL);
  unit = xmlNewChild(node, NULL, BAD_CAST "dopplerCentroidLimit", 
		     fStr(quality->dopplerCentroidLimit));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  unit = xmlNewChild(node, NULL, BAD_CAST "geolocationQualityLimit",
		     fStr(quality->geolocationQualityLimit));
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "arcsec");

  quality->instrumentStateRemark = (char *) MALLOC(sizeof(char)*1024);
  strcpy(quality->instrumentStateRemark, MAGIC_UNSET_STRING);

  if (quality->instrumentStateRemark)
    xmlNewChild(section, NULL, BAD_CAST "instrumentStateRemark", 
		BAD_CAST quality->instrumentStateRemark);

  // Save tree to file
  xmlSaveFormatFileEnc(outFile, doc, "UTF-8", 1);

  // Clean up
  xmlFreeDoc(doc);
  xmlCleanupParser();
}
