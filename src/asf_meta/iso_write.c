#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include "asf_iso_meta.h"
#include "asf_nan.h"
#include "xml_util.h"

static void date2str(iso_dateTime dateUTC, char *str)
{
  int year = dateUTC.year;
  int month = dateUTC.month;
  int day = dateUTC.day;
  
  // UTC date stamp: 2008-03-13
  if (year < 0 || month < 0 || day < 0)
    strcpy(str, "1900-01-01");
  else
    sprintf(str, "%4d-%02d-%02d", year, month, day);
}

static void dateTime2str(iso_dateTime timeUTC, char *str)
{
  int year = timeUTC.year;
  int month = timeUTC.month;
  int day = timeUTC.day;
  int hour = timeUTC.hour;
  int min = timeUTC.min;
  double sec = timeUTC.second;

  // UTC time stamp: 2008-03-13T22:19:55.140975
  if (year < 0 || month < 0 || day < 0 || hour < 0 || min < 0 || sec < 0.0)
    strcpy(str, "1900-01-01T00:00:00.000000Z");
  else
    sprintf(str, "%4d-%02d-%02dT%02d:%02d:%09.6fZ", 
	    year, month, day, hour, min, sec);
}

void boolean2str(int flag, char *str)
{
  if (flag)
    strcpy(str, "true");
  else
    strcpy(str, "false");
}

void double2str(double value, int decimals, char *str)
{
  if (ISNAN(value))
    strcpy(str, "NaN");
  else if (decimals == 0)
    sprintf(str, "%g", value);
  else {
    char format[10];
    sprintf(format, "%%.%dlf", decimals);
    sprintf(str, format, value);
  }
}

void iso_meta_write(iso_meta *iso, const char *gapFile, const char *outFile)
{
  unsigned long ii, kk;
  char *str;
  
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
  str = (char *) MALLOC(sizeof(char)*1024);

  // General Header
  iso_generalHeader *header = iso->generalHeader;
  section = xmlNewChild(root, NULL, BAD_CAST "generalHeader", NULL);
  xmlNewProp(section, BAD_CAST "fileName", BAD_CAST outFile);
  xmlNewProp(section, BAD_CAST "fileVersion", BAD_CAST "1.0");
  xmlNewProp(section, BAD_CAST "status", BAD_CAST "PRELIMINARY");
  xmlNewChild(section, NULL, BAD_CAST "itemName", BAD_CAST header->itemName);
  xmlNewChild(section, NULL, BAD_CAST "mission", BAD_CAST header->mission);

  // Read the source information out of the .roi.in file
  if (gapFile) {
    char line[512], *p=NULL, source[255];
    char *disFile = (char *) MALLOC(sizeof(char)*(strlen(gapFile)+10));
    disFile = appendExt(gapFile, ".roi.in");
    FILE *fp = FOPEN(disFile, "r");
    if (fp) {
      while (fgets(line, 512, fp)) {
	if (strstr(line, "First input data file")) {
	  p = strchr(line, ' ');
	  if (p)
	    *p = '\0';
	  sprintf(source, "%s", line);
	  xmlNewChild(section, NULL, BAD_CAST "source", BAD_CAST line);
	}
      }
    }
    FCLOSE(fp);
    FREE(disFile);
  }
  else
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
      sprintf(str, "%ld", comps->annotation[ii].file.size);
      xmlNewChild(parent, NULL, BAD_CAST "size", BAD_CAST str);
    }
  }
  if (comps->imageData) {
    for (ii=0; ii<comps->numLayers; ii++) {
      parent = xmlNewChild(section, NULL, BAD_CAST "imageData", NULL);
      sprintf(str, "%ld", ii+1);
      xmlNewProp(parent, BAD_CAST "layerIndex", BAD_CAST str);
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
      sprintf(str, "%ld", comps->imageData[ii].file.size);
      xmlNewChild(node, NULL, BAD_CAST "size", BAD_CAST str);
    }
  }
  if (comps->auxRasterFiles) {
    // FIXME: fill as needed
  }
  if (comps->quicklooks) {
    for (ii=0; ii<comps->numLayers; ii++) {
      parent = xmlNewChild(section, NULL, BAD_CAST "quicklooks", NULL);
      sprintf(str, "%ld", ii+1);
      xmlNewProp(parent, BAD_CAST "layerIndex", BAD_CAST str);
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
      sprintf(str, "%ld", comps->quicklooks[ii].file.size);
      xmlNewChild(node, NULL, BAD_CAST "size", BAD_CAST str);
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
  sprintf(str, "%ld", comps->browseImage.size);
  xmlNewChild(parent, NULL, BAD_CAST "size", BAD_CAST str);
  parent = xmlNewChild(section, NULL, BAD_CAST "mapPlot", NULL);
  node = xmlNewChild(parent, NULL, BAD_CAST "file", NULL);
  parent = node;
  node = xmlNewChild(parent, NULL, BAD_CAST "location", NULL);
  xmlNewChild(node, NULL, BAD_CAST "host", BAD_CAST comps->mapPlot.host);
  xmlNewChild(node, NULL, BAD_CAST "path", BAD_CAST comps->mapPlot.path);
  xmlNewChild(node, NULL, BAD_CAST "filename", BAD_CAST comps->mapPlot.name);
  sprintf(str, "%ld", comps->mapPlot.size);
  xmlNewChild(parent, NULL, BAD_CAST "size", BAD_CAST str);
  
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
  sprintf(str, "%d", info->orbitPhase);
  xmlNewChild(parent, NULL, BAD_CAST "orbitPhase", BAD_CAST str);
  sprintf(str, "%d", info->orbitCycle);
  xmlNewChild(parent, NULL, BAD_CAST "orbitCycle", BAD_CAST str);
  sprintf(str, "%d", info->absOrbit);
  xmlNewChild(parent, NULL, BAD_CAST "absOrbit", BAD_CAST str);
  sprintf(str, "%d", info->relOrbit);
  xmlNewChild(parent, NULL, BAD_CAST "relOrbit", BAD_CAST str);
  sprintf(str, "%d", info->numOrbitsInCycle);
  xmlNewChild(parent, NULL, BAD_CAST "numOrbitsInCycle", BAD_CAST str);
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
  xmlNewChild(parent, NULL, BAD_CAST "imagingMode", BAD_CAST str);
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
    sprintf(str, "%d", info->numberOfBeams);
    xmlNewChild(parent, NULL, BAD_CAST "numberOfBeams", BAD_CAST str);
    xmlNodePtr beams = xmlNewChild(parent, NULL, BAD_CAST "beamList", NULL);
    for (ii=0; ii<info->numberOfBeams; ii++)
      xmlNewChild(beams, NULL, BAD_CAST "beamID", BAD_CAST info->beamID[ii]);
    xmlNewChild(parent, NULL, BAD_CAST "azimuthBeamID", 
		BAD_CAST info->azimuthBeamID);
    sprintf(str, "%d", info->numberOfBursts);
    xmlNewChild(parent, NULL, BAD_CAST "numberOfBursts", BAD_CAST str);
  }
  else if (info->imageMode == SPOTLIGHT_IMAGE) {
    parent = xmlNewChild(node, NULL, BAD_CAST "spotLight", NULL);
    sprintf(str, "%d", info->numberOfAzimuthBeams);
    xmlNewChild(parent, NULL, BAD_CAST "numberOfAzimuthBeams", BAD_CAST str);
    xmlNewChild(parent, NULL, BAD_CAST "azimuthBeamIDFirst", 
		BAD_CAST info->azimuthBeamIDFirst);
    xmlNewChild(parent, NULL, BAD_CAST "azimuthBeamIDLast", 
		BAD_CAST info->azimuthBeamIDLast);
    double2str(info->azimuthSteeringAngleFirst, 5, str);
    unit = xmlNewChild(parent, NULL, BAD_CAST "azimuthSteeringAngleFirst",
		       BAD_CAST str);
    xmlNewProp(node, BAD_CAST "units", BAD_CAST "degrees");
    double2str(info->azimuthSteeringAngleLast, 5, str);
    unit = xmlNewChild(parent, NULL, BAD_CAST "azimuthSteeringAngleLast",
		       BAD_CAST str);
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
    sprintf(str, "HDF5");
  else if (info->imageDataFormat == COSAR_DATA_FORMAT)
    strcpy(str, "COSAR");
  else if (info->imageDataFormat == UNDEF_DATA_FORMAT)
    strcpy(str, "UNDEFINED");
  node = xmlNewChild(parent, NULL, BAD_CAST "imageDataFormat", BAD_CAST str);
  sprintf(str, "%d.%d.%d", H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
  xmlNewProp(node, BAD_CAST "version", BAD_CAST str);
  sprintf(str, "%d", info->numberOfLayers);
  xmlNewChild(parent, NULL, BAD_CAST "numberOfLayers", BAD_CAST str);
  sprintf(str, "%d", info->imageDataDepth);
  xmlNewChild(parent, NULL, BAD_CAST "imageDataDepth", BAD_CAST str);
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
  sprintf(str, "%d", info->numberOfRows);
  xmlNewChild(node, NULL, BAD_CAST "numberOfRows", BAD_CAST str);
  sprintf(str, "%d", info->numberOfColumns);
  xmlNewChild(node, NULL, BAD_CAST "numberOfColumns", BAD_CAST str);
  sprintf(str, "%i", info->startRow);
  xmlNewChild(node, NULL, BAD_CAST "startRow", BAD_CAST str);
  sprintf(str, "%i", info->startColumn);
  xmlNewChild(node, NULL, BAD_CAST "startColumn", BAD_CAST str);
  double2str(info->rowScaling, 0, str);
  xmlNewChild(node, NULL, BAD_CAST "rowScaling", BAD_CAST str);
  double2str(info->columnScaling, 0, str);
  xmlNewChild(node, NULL, BAD_CAST "columnScaling", BAD_CAST str);
  double2str(info->rowSpacing, 10, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "rowSpacing", BAD_CAST str);
  if (info->projection == MAP_PROJ)
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");  
  else
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  double2str(info->columnSpacing, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "columnSpacing", BAD_CAST str);
  if (info->projection == MAP_PROJ)
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");  
  else
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  double2str(info->groundRangeResolution, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "groundRangeResolution", 
		     BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(info->azimuthResolution, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "azimuthResolution", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(info->azimuthLooks, 0, str);
  xmlNewChild(node, NULL, BAD_CAST "azimuthLooks", BAD_CAST str);
  double2str(info->rangeLooks, 0, str);
  xmlNewChild(node, NULL, BAD_CAST "rangeLooks", BAD_CAST str);
  
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
  double2str(info->rangeTimeFirstPixel, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "firstPixel", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  double2str(info->rangeTimeLastPixel, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "lastPixel", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  double2str(info->sceneAzimuthExtent, 3, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "sceneAzimuthExtent", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(info->sceneRangeExtent, 3, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "sceneRangeExtent", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  node = xmlNewChild(parent, NULL, BAD_CAST "sceneCenterCoord", NULL);
  sprintf(str, "%i", info->sceneCenterCoord.refRow);
  xmlNewChild(node, NULL, BAD_CAST "refRow", BAD_CAST str);
  sprintf(str, "%i", info->sceneCenterCoord.refColumn);
  xmlNewChild(node, NULL, BAD_CAST "refColumn", BAD_CAST str);
  double2str(info->sceneCenterCoord.lat, 5, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "lat", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  double2str(info->sceneCenterCoord.lon, 5, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "lon", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  dateTime2str(info->sceneCenterCoord.azimuthTimeUTC, str);
  xmlNewChild(node, NULL, BAD_CAST "azimuthTimeUTC", BAD_CAST str);
  double2str(info->sceneCenterCoord.rangeTime, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "rangeTime", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  double2str(info->sceneCenterCoord.incidenceAngle, 5, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "incidenceAngle", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  double2str(info->sceneAverageHeight, 3, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "sceneAverageHeight", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  for (ii=0; ii<4; ii++) {
    node = xmlNewChild(parent, NULL, BAD_CAST "sceneCornerCoord", NULL);
    sprintf(str, "%i", info->sceneCornerCoord[ii].refRow);
    xmlNewChild(node, NULL, BAD_CAST "refRow", BAD_CAST str);
    sprintf(str, "%i", info->sceneCornerCoord[ii].refColumn);
    xmlNewChild(node, NULL, BAD_CAST "refColumn",BAD_CAST str);
    double2str(info->sceneCornerCoord[ii].lat, 5, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "lat", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(info->sceneCornerCoord[ii].lon, 5, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "lon", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    dateTime2str(info->sceneCornerCoord[ii].azimuthTimeUTC, str);
    xmlNewChild(node, NULL, BAD_CAST "azimuthTimeUTC", BAD_CAST str);
    double2str(info->sceneCornerCoord[ii].rangeTime, 0, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "rangeTime", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
    double2str(info->sceneCornerCoord[ii].incidenceAngle, 5, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "incidenceAngle", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  }
  double2str(info->yaw, 5, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "yawAngle", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  double2str(info->pitch, 5, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "pitchAngle", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  double2str(info->roll, 5, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "rollAngle", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  double2str(info->headingAngle, 5, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "headingAngle", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  double2str(info->earthRadius, 3, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "earthRadius", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(info->satelliteHeight, 3, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "satelliteHeight", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  
  parent = xmlNewChild(section, NULL, BAD_CAST "previewInfo", NULL);
  node = xmlNewChild(parent, NULL, BAD_CAST "quicklooks", NULL);
  xmlNewChild(node, NULL, BAD_CAST "imageDataFormat", 
	      BAD_CAST info->quicklooks.imageDataFormat);
  sprintf(str, "%d", info->quicklooks.imageDataDepth);
  xmlNewChild(node, NULL, BAD_CAST "imageDataDepth", BAD_CAST str);
  node2 = xmlNewChild(node, NULL, BAD_CAST "imageRaster", NULL);
  sprintf(str, "%d", info->quicklooks.numberOfRows);
  xmlNewChild(node2, NULL, BAD_CAST "numberOfRows", BAD_CAST str);
  sprintf(str, "%d", info->quicklooks.numberOfColumns);
  xmlNewChild(node2, NULL, BAD_CAST "numberOfColumns", BAD_CAST str);
  double2str(info->quicklooks.columnBlockLength, 0, str);
  unit = xmlNewChild(node2, NULL, BAD_CAST "columnBlockLength", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "pixels");
  double2str(info->quicklooks.rowBlockLength, 0, str);
  unit = xmlNewChild(node2, NULL, BAD_CAST "rowBlockLength", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "pixels");
  double2str(info->quicklooks.rowSpacing, 0, str);
  unit = xmlNewChild(node2, NULL, BAD_CAST "rowSpacing", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(info->quicklooks.columnSpacing, 0, str);
  unit = xmlNewChild(node2, NULL, BAD_CAST "columnSpacing", BAD_CAST str);
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
  sprintf(str, "%d", info->browseImageDataDepth);
  xmlNewChild(node, NULL, BAD_CAST "imageDataDepth", BAD_CAST str);
  xmlNewChild(parent, NULL, BAD_CAST "mapPlotFormat", 
	      BAD_CAST info->mapPlotFormat);
  
  // Product Specific
  iso_productSpecific *spec = iso->productSpecific;
  section = xmlNewChild(root, NULL, BAD_CAST "productSpecific", NULL);
  parent = xmlNewChild(section, NULL, BAD_CAST "complexImageInfo", NULL);
  double2str(spec->commonPRF, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "commonPRF", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  double2str(spec->commonRSF, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "commonRSF", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  double2str(spec->slantRangeResolution, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "slantRangeResolution", 
		     BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(spec->projectedSpacingAzimuth, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "projectedSpacingAzimuth",
		     BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  node = xmlNewChild(parent, NULL, BAD_CAST "projectedSpacingRange", NULL);
  double2str(spec->projectedSpacingGroundNearRange, 3, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "groundNear", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(spec->projectedSpacingGroundFarRange, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "groundFar", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(spec->projectedSpacingSlantRange, 3, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "slantRange", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
  double2str(spec->slantRangeShift, 3, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "slantRangeShift", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
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
    double2str(spec->projectionCenterLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "latitude", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->projectionCenterLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "longitude", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    node2 = xmlNewChild(node, NULL, BAD_CAST "mapOrigin", NULL);
    double2str(spec->mapOriginEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "easting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->mapOriginNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "northing", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->scaleFactor, 0, str);
    xmlNewChild(node, NULL, BAD_CAST "scaleFactor", BAD_CAST str);
    node  = xmlNewChild(parent, NULL, BAD_CAST "geoParameter", NULL);
    node2 = xmlNewChild(node, NULL, BAD_CAST "pixelSpacing", NULL);
    double2str(spec->pixelSpacingEasting, 0, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "easting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->pixelSpacingNorthing, 0, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "northing", BAD_CAST str);
    node2 = xmlNewChild(node, NULL, BAD_CAST "frameCoordsGeographic", NULL);
    double2str(spec->frameCoordsGeographic.upperLeftLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftLatitude", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->frameCoordsGeographic.upperLeftLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftLongitude", 
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->frameCoordsGeographic.upperRightLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightLatitude", 
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->frameCoordsGeographic.upperRightLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightLongitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->frameCoordsGeographic.lowerLeftLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftLatitude", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->frameCoordsGeographic.lowerLeftLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftLongitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->frameCoordsGeographic.lowerRightLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightLatitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->frameCoordsGeographic.lowerRightLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightLongitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    node2 = xmlNewChild(node, NULL, BAD_CAST "frameCoordsCartographic", NULL);
    double2str(spec->frameCoordsCartographic.upperLeftEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftEasting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->frameCoordsCartographic.upperLeftNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftNorthing", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->frameCoordsCartographic.upperRightEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightEasting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->frameCoordsCartographic.upperRightNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightNorthing",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->frameCoordsCartographic.lowerRightEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightEasting",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->frameCoordsCartographic.lowerRightNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightNorthing",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->frameCoordsCartographic.lowerLeftEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftEasting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->frameCoordsCartographic.lowerLeftNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftNorthing", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCoordsGeographic", NULL);
    double2str(spec->sceneCoordsGeographic.upperLeftLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftLatitude", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->sceneCoordsGeographic.upperLeftLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftLongitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->sceneCoordsGeographic.upperRightLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightLatitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->sceneCoordsGeographic.upperRightLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightLongitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->sceneCoordsGeographic.lowerLeftLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftLatitude", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->sceneCoordsGeographic.lowerLeftLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftLongitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->sceneCoordsGeographic.lowerRightLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightLatitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->sceneCoordsGeographic.lowerRightLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightLongitude", 
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCoordsCartographic", NULL);
    double2str(spec->sceneCoordsCartographic.upperLeftEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftEasting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->sceneCoordsCartographic.upperLeftNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperLeftNorthing", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->sceneCoordsCartographic.upperRightEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightEasting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->sceneCoordsCartographic.upperRightNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "upperRightNorthing", 
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->sceneCoordsCartographic.lowerRightEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightEasting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->sceneCoordsCartographic.lowerRightNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerRightNorthing", 
		       BAD_CAST str); 
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->sceneCoordsCartographic.lowerLeftEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftEasting", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->sceneCoordsCartographic.lowerLeftNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lowerLeftNorthing", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCenterCoordsGeographic", 
			NULL);
    double2str(spec->sceneCenterCoordLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "centerCoordLatitude", 
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(spec->sceneCenterCoordLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "centerCoordLongitude",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCenterCoordsCartograhic", 
			NULL);
    double2str(spec->sceneCenterCoordEasting, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "centerCoordEasting", 
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(spec->sceneCenterCoordNorthing, 3, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "centerCoordNorthing",
		       BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    if (spec->imageResamplingMethod == NEAREST_NEIGHBOR_RESAMPLE)
      strcpy(str, "NEAREST NEIGHBOR");
    else if (spec->imageResamplingMethod == BILINEAR_RESAMPLE)
      strcpy(str, "BILINEAR");
    else if (spec->imageResamplingMethod == CUBIC_CONVOLUTION_RESAMPLE)
      strcpy(str, "CUBIC CONVOLUTION");
    else if (spec->imageResamplingMethod == UNDEF_RESAMPLE)
      strcpy(str, "UNDEFINED");
    xmlNewChild(node, NULL, BAD_CAST "imageResamplingMethod", BAD_CAST str);
    if (spec->elevationDataFlag) {
      node = xmlNewChild(parent, NULL, BAD_CAST "elevationData", NULL);
      xmlNewChild(node, NULL, BAD_CAST "dataSource", 
		  BAD_CAST spec->elevationDataSource);
      double2str(spec->elevationMinimumHeight, 3, str);
      unit = xmlNewChild(node, NULL, BAD_CAST "minimumHeight", BAD_CAST str);
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
      double2str(spec->elevationMeanHeight, 3, str);
      unit = xmlNewChild(node, NULL, BAD_CAST "meanHeight", BAD_CAST str);
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
      double2str(spec->elevationMaximumHeight, 3, str);
      unit = xmlNewChild(node, NULL, BAD_CAST "maximumHeight", BAD_CAST str);
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
      sprintf(str, "%d", spec->incidenceAngleImageDataDepth);
      xmlNewChild(node, NULL, BAD_CAST "imageDataDepth", BAD_CAST str);
      node2 = xmlNewChild(node, NULL, BAD_CAST "imageRaster", NULL);
      sprintf(str, "%d", spec->incidenceAngleNumberOfRows);
      xmlNewChild(node2, NULL, BAD_CAST "numberOfRows", BAD_CAST str);
      sprintf(str, "%d", spec->incidenceAngleNumberOfColumns);
      xmlNewChild(node2, NULL, BAD_CAST "numberOfColumns", BAD_CAST str);
      double2str(spec->incidenceAngleRowSpacing, 0, str);
      unit = xmlNewChild(node2, NULL, BAD_CAST "rowSpacing", BAD_CAST str);
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
      double2str(spec->incidenceAngleColumnSpacing, 0, str);
      unit = xmlNewChild(node2, NULL, BAD_CAST "columnSpacing", BAD_CAST str);
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
  if (setup->sceneSpecification == FRAME_SPEC) {
    sprintf(str, "%d", setup->frameID);
    xmlNewChild(node, NULL, BAD_CAST "frameID", BAD_CAST str);
  }
  else if (setup->sceneSpecification == TIME_SPEC) {
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneExtent", NULL);
    dateTime2str(setup->sceneStartTimeUTC, str);
    xmlNewChild(node2, NULL, BAD_CAST "startTimeUTC", BAD_CAST str);
    dateTime2str(setup->sceneStopTimeUTC, str);
    xmlNewChild(node2, NULL, BAD_CAST "stopTimeUTC", BAD_CAST str);
  }
  else if (setup->sceneSpecification == CENTERCOORDS_SPEC) {
    node2 = xmlNewChild(node, NULL, BAD_CAST "sceneCenterCoord", NULL);
    double2str(setup->sceneCenterLatitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lat", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
    double2str(setup->sceneCenterLongitude, 5, str);
    unit = xmlNewChild(node2, NULL, BAD_CAST "lon", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "degrees");
  }
  if (setup->imagingMode == FINE_BEAM)
    strcpy(str, "FINE BEAM");
  else if (setup->imagingMode == STANDARD_BEAM)
    strcpy(str, "STANDARD BEAM");
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
  node2 = xmlNewChild(node, NULL, BAD_CAST "polList", NULL);
  xmlNewChild(node2, NULL, BAD_CAST "polLayer", BAD_CAST str);
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
      dateTime2str(setup->processingStep[ii].processingTimeUTC, str);
      xmlNewChild(node, NULL, BAD_CAST "processingTimeUTC", BAD_CAST str);  
      xmlNewChild(node, NULL, BAD_CAST "description", 
		  BAD_CAST setup->processingStep[ii].description);
      xmlNewChild(node, NULL, BAD_CAST "algorithm", 
		  BAD_CAST setup->processingStep[ii].algorithm);
      if (setup->processingStep[ii].processingLevel == PRE_PROCESSING)
	xmlNewChild(node, NULL, BAD_CAST "processingLevel",
		    BAD_CAST "PRE-PROCESSING");
      else if (setup->processingStep[ii].processingLevel == LEVEL_ZERO)
	xmlNewChild(node, NULL, BAD_CAST "processingLevel",
		    BAD_CAST "LEVEL ZERO");
      else if (setup->processingStep[ii].processingLevel == LEVEL_ONE)
	xmlNewChild(node, NULL, BAD_CAST "processingLevel",
		    BAD_CAST "LEVEL ONE");
      else if (setup->processingStep[ii].processingLevel == LEVEL_TWO)
	xmlNewChild(node, NULL, BAD_CAST "processingLevel",
		    BAD_CAST "LEVEL TWO");
      else if (setup->processingStep[ii].processingLevel == UNDEF_PROC_LEVEL)
	xmlNewChild(node, NULL, BAD_CAST "processingLevel",
		    BAD_CAST "UNDEFINED");
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
      sprintf(str, "%ld", ii+1);
      xmlNewProp(node, BAD_CAST "layerIndex", BAD_CAST str);
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
      sprintf(str, "%d", proc->doppler[ii].numberOfBlocks);
      xmlNewChild(node, NULL, BAD_CAST "numberOfBlocks", BAD_CAST str);
      sprintf(str, "%d", proc->doppler[ii].numberOfRejectedBlocks);
      xmlNewChild(node, NULL, BAD_CAST "numberOfRejectedBlocks", BAD_CAST str);
      sprintf(str, "%d", proc->doppler[ii].numberOfDopperRecords);
      xmlNewChild(node, NULL, BAD_CAST "numberOfDopplerRecords", BAD_CAST str);
      node2 = xmlNewChild(node, NULL, BAD_CAST "dopplerEstimate", NULL);
      dateTime2str(proc->doppler[ii].timeUTC, str);
      xmlNewChild(node2, NULL, BAD_CAST "timeUTC", BAD_CAST str);
      double2str(proc->doppler[ii].dopplerAtMidRange, 0, str);
      xmlNewChild(node2, NULL, BAD_CAST "dopplerAtMidRange", BAD_CAST str);
      int degree = proc->doppler[ii].polynomialDegree;
      sprintf(str, "%d", degree);
      xmlNewChild(node2, NULL, BAD_CAST "polynomialDegree", BAD_CAST str);
      node3 = xmlNewChild(node2, NULL, BAD_CAST "basebandDoppler", NULL);
      for (kk=0; kk<=degree; kk++) {
	double2str(proc->doppler[ii].coefficient[kk], 0, str);
	unit = xmlNewChild(node3, NULL, BAD_CAST "coefficient", BAD_CAST str);
	sprintf(str, "%ld", kk);
	xmlNewProp(unit, BAD_CAST "exponent", BAD_CAST str);
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
  double2str(proc->processingParameter[0].rangeLooks, 0, str);
  xmlNewChild(parent, NULL, BAD_CAST "rangeLooks", BAD_CAST str);
  double2str(proc->processingParameter[0].azimuthLooks, 0, str);
  xmlNewChild(parent, NULL, BAD_CAST "azimuthLooks", BAD_CAST str);
  double2str(proc->processingParameter[0].rangeLookBandwidth, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "rangeLookBandwidth", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  double2str(proc->processingParameter[0].azimuthLookBandwidth, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "azimuthLookBandwidth", 
		     BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  double2str(proc->processingParameter[0].totalProcessedRangeBandwidth, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "totalProcessedRangeBandwidth",
		     BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  double2str(proc->processingParameter[0].totalProcessedAzimuthBandwidth, 0, 
	     str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "totalProcessedAzimuthBandwidth",
		     BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  double2str(proc->processingParameter[0].chirpRate, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "chirpRate", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz/s");
  double2str(proc->processingParameter[0].pulseDuration, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "pulseDuration", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");


  parent = xmlNewChild(section, NULL, BAD_CAST "processingFlags", NULL);
  boolean2str(proc->chirpReplicaUsedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "chirpReplicaUsedFlag", BAD_CAST str);
  boolean2str(proc->geometricDopplerUsedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "geometricDopplerUsedFlag", BAD_CAST str);
  boolean2str(proc->azimuthPatternCorrectedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "azimuthPatternCorrectedFlag", 
	      BAD_CAST str);
  boolean2str(proc->elevationPatternCorrectedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "elevationPatternCorrectedFlag",
	      BAD_CAST str);
  boolean2str(proc->detectedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "detectedFlag", BAD_CAST str);
  boolean2str(proc->multiLookedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "multiLookedFlag", BAD_CAST str);
  boolean2str(proc->polarimetricProcessedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "polarimetricProcessedFlag", BAD_CAST str);
  boolean2str(proc->terrainCorrectedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "terrainCorrectedFlag", BAD_CAST str);
  boolean2str(proc->layoverShadowMaskGeneratedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "layoverShadowMaskGeneratedFlag",
	      BAD_CAST str);
  boolean2str(proc->geocodedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "geocodedFlag", BAD_CAST str);
  boolean2str(proc->nominalProcessingPerformedFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "nominalProcessingPerformedFlag", 
	      BAD_CAST str);

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
  double2str(inst->centerFrequency, 0, str);
  unit = xmlNewChild(parent, NULL, BAD_CAST "centerFrequency", BAD_CAST str);
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
      double2str(inst->settings[ii].rxBandwidth, 0, str);
      unit = xmlNewChild(parent, NULL, BAD_CAST "rxBandwidth", BAD_CAST str);
      xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
      double2str(inst->settings[ii].rsf, 0, str);
      unit = xmlNewChild(parent, NULL, BAD_CAST "RSF", BAD_CAST str);
      
      inst->settings[ii].numberOfPRFChanges = 0;
      inst->settings[ii].numberOfEchoWindowPositionChanges = 0;
      inst->settings[ii].numberOfEchoWindowLengthChanges = 0;
      inst->settings[ii].numberOfSettingRecords = 1;
      
      sprintf(str, "%d", inst->settings[ii].numberOfPRFChanges);
      xmlNewChild(parent, NULL, BAD_CAST "numberOfPRFChanges", BAD_CAST str);
      sprintf(str, "%d", inst->settings[ii].numberOfEchoWindowPositionChanges);
      xmlNewChild(parent, NULL, BAD_CAST "numberOfEchoWindowPositionChanges",
		  BAD_CAST str);
      sprintf(str, "%d", inst->settings[ii].numberOfEchoWindowLengthChanges);
      xmlNewChild(parent, NULL, BAD_CAST "numberOfEchoWindowLengthChanges",
		  BAD_CAST str);
      sprintf(str, "%d", inst->settings[ii].numberOfSettingRecords);
      xmlNewChild(parent, NULL, BAD_CAST "numberOfSettingRecords", 
		  BAD_CAST str);
      node = xmlNewChild(parent, NULL, BAD_CAST "settingRecords", NULL);
      iso_settingRecord *rec = inst->settings[ii].settingRecord;
      for (kk=0; kk<inst->settings[ii].numberOfSettingRecords; kk++) {
	node2 = xmlNewChild(node, NULL, BAD_CAST "dataSegment", NULL);
	sprintf(str, "%ld", kk+1);
	xmlNewProp(node2, BAD_CAST "segmentID", BAD_CAST str);
	dateTime2str(rec[kk].startTimeUTC, str);
	xmlNewChild(node2, NULL, BAD_CAST "startTimeUTC", BAD_CAST str);
	dateTime2str(rec[kk].stopTimeUTC, str);
	xmlNewChild(node2, NULL, BAD_CAST "stopTimeUTC", BAD_CAST str);
	sprintf(str, "%d", rec[kk].numberOfRows);
	xmlNewChild(node2, NULL, BAD_CAST "numberOfRows", BAD_CAST str);
	double2str(rec[kk].prf,0, str);
	unit = xmlNewChild(node, NULL, BAD_CAST "PRF", BAD_CAST str);
	xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
	double2str(rec[kk].echoWindowPosition, 0, str); 
	xmlNewChild(node, NULL, BAD_CAST "echoWindowPosition", BAD_CAST str);
	double2str(rec[kk].echoWindowLength, 0, str); 
	xmlNewChild(node, NULL, BAD_CAST "echoWindowLength", BAD_CAST str);
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
    strcpy(str, "PREDICTED SENSOR");
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

  sprintf(str, "%ld", platform->numStateVectors);
  xmlNewChild(node, NULL, BAD_CAST "numStateVectors", BAD_CAST str);
  dateTime2str(platform->firstStateTimeUTC, str);
  xmlNewChild(node, NULL, BAD_CAST "firstStateTimeUTC", BAD_CAST str);
  dateTime2str(platform->lastStateTimeUTC, str);
  xmlNewChild(node, NULL, BAD_CAST "lastStateTimeUTC", BAD_CAST str);
  xmlNewChild(node, NULL, BAD_CAST "stateVectorRefFrame",
	      BAD_CAST platform->stateVectorRefFrame);
  double2str(platform->stateVectorTimeSpacing, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "stateVectorTimeSpacing", 
		     BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "s");
  char num[5];
  for (ii=0; ii<platform->numStateVectors; ii++) {
    node = xmlNewChild(parent, NULL, BAD_CAST "stateVec", NULL);
    sprintf(num, "%ld", ii+1); 
    xmlNewProp(node, BAD_CAST "num", BAD_CAST num);
    dateTime2str(platform->stateVec[ii].timeUTC, str);
    xmlNewChild(node, NULL, BAD_CAST "timeUTC", BAD_CAST str);
    double2str(platform->stateVec[ii].posX, 3, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "posX", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(platform->stateVec[ii].posY, 3, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "posY", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(platform->stateVec[ii].posZ, 3, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "posZ", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m");
    double2str(platform->stateVec[ii].velX, 3, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "velX", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m/s");
    double2str(platform->stateVec[ii].velY, 3, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "velY", BAD_CAST str);
    xmlNewProp(unit, BAD_CAST "units", BAD_CAST "m/s");
    double2str(platform->stateVec[ii].velZ, 3, str);
    unit = xmlNewChild(node, NULL, BAD_CAST "velZ", BAD_CAST str);
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

      // gap information already in the metadata
      if (numGaps > 0) {
        sprintf(str, "%d", numGaps);
        xmlNewChild(parent, NULL, BAD_CAST "numGaps", BAD_CAST str);
        for (kk=0; kk<numGaps; kk++) {
  	  node = xmlNewChild(parent, NULL, BAD_CAST "gap", NULL);
 	  sprintf(num, "%ld", kk+1);
	  xmlNewProp(node, BAD_CAST "num", BAD_CAST num);
	  sprintf(str, "%ld", quality->rawDataQuality[ii].gap[kk].start);
	  xmlNewChild(node, NULL, BAD_CAST "start", BAD_CAST str);
	  sprintf(str, "%d", quality->rawDataQuality[ii].gap[kk].length);
	  xmlNewChild(node, NULL, BAD_CAST "length", BAD_CAST str);
	  if (quality->rawDataQuality[ii].gap[kk].fill == RANDOM_FILL)
	    strcpy(str, "RANDOM");
	  else if (quality->rawDataQuality[ii].gap[kk].fill == ZERO_FILL)
	    strcpy(str, "ZERO");
	  else if (quality->rawDataQuality[ii].gap[kk].fill == UNDEF_FILL)
	    strcpy(str, "UNDEFINED");
	  xmlNewChild(node, NULL, BAD_CAST "fill", BAD_CAST str);
        }
      }

      // gap information from SEASAT processing .dis file
      else if (gapFile && ii==0 && numGaps == 0) {
        if (!fileExists(gapFile)) {
          printf("Gaps file not found: %s\n", gapFile);
          xmlNewChild(parent, NULL, BAD_CAST "numGaps", BAD_CAST "0");
        }
        else {
          FILE *fp = fopen(gapFile, "r");
          if (!fp) {
            printf("Gaps file could not be opened: %s\n", gapFile);
            xmlNewChild(parent, NULL, BAD_CAST "numGaps", BAD_CAST "0");
          }
          else {
            printf("Found gaps file: %s\n", gapFile);
            numGaps = 0;
            char str[1024];
            int lines[100], gaps[100];
            fgets(str, 1024, fp);
            while (fgets(str, 1024, fp) && numGaps < 100) {
              int line, gap;
              if (sscanf(str, "%d\t%d\n", &line, &gap) == 2) {
                lines[numGaps] = line;
                gaps[numGaps] = gap;
                ++numGaps;
              }
            }
            fclose(fp);
            printf("Found %d gaps in file: %s\n", numGaps, gapFile);
        
            sprintf(str, "%d", numGaps);
            xmlNewChild(parent, NULL, BAD_CAST "numGaps", BAD_CAST str);

            for (kk=0; kk<numGaps; ++kk) {
  	      node = xmlNewChild(parent, NULL, BAD_CAST "gap", NULL);
   	      sprintf(num, "%ld", kk+1);
	      xmlNewProp(node, BAD_CAST "num", BAD_CAST num);
	      sprintf(str, "%ld", lines[kk]);
	      xmlNewChild(node, NULL, BAD_CAST "start", BAD_CAST str);
	      sprintf(str, "%d", gaps[kk]);
	      xmlNewChild(node, NULL, BAD_CAST "length", BAD_CAST str);
	      xmlNewChild(node, NULL, BAD_CAST "fill", BAD_CAST "RANDOM");
            }
          }
        }
      }
      
      // no gap information
      else {
        xmlNewChild(parent, NULL, BAD_CAST "numGaps", BAD_CAST "0");
      }

      boolean2str(quality->rawDataQuality[ii].gapSignificanceFlag, str);
      xmlNewChild(parent, NULL, BAD_CAST "gapSignificanceFlag", BAD_CAST str);
      boolean2str(quality->rawDataQuality[ii].missingLinesSignificanceFlag, 
		  str);
      xmlNewChild(parent, NULL, BAD_CAST "missingLinesSignificanceFlag",
		  BAD_CAST str);
      boolean2str(quality->rawDataQuality[ii].bitErrorSignificanceFlag, str);
      xmlNewChild(parent, NULL, BAD_CAST "bitErrorSignificanceFlag", 
		  BAD_CAST str);
      boolean2str(quality->rawDataQuality[ii].timeReconstructionSignificanceFlag, str);
      xmlNewChild(parent, NULL, BAD_CAST "timeReconstructionSignificanceFlag",
		  BAD_CAST str);
    }
  }
  parent = xmlNewChild(section, NULL, BAD_CAST "processingParameterQuality", 
		       NULL);
  boolean2str(quality->dopplerAmbiguityNotZeroFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "dopplerAmbiguityNotZeroFlag", 
	      BAD_CAST str);
  boolean2str(quality->dopplerOutsideLimitsFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "dopplerOutsideLimitsFlag", BAD_CAST str);
  boolean2str(quality->geolocationQualityLowFlag, str);
  xmlNewChild(parent, NULL, BAD_CAST "geolocationQualityLowFlag", BAD_CAST str);
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
      double2str(quality->imageDataQuality[ii].min, 0, str); 
      xmlNewChild(node, NULL, BAD_CAST "minValue", BAD_CAST str);
      double2str(quality->imageDataQuality[ii].max, 0, str); 
      xmlNewChild(node, NULL, BAD_CAST "maxValue", BAD_CAST str);
      double2str(quality->imageDataQuality[ii].mean, 0, str); 
      xmlNewChild(node, NULL, BAD_CAST "meanValue", BAD_CAST str);
      double2str(quality->imageDataQuality[ii].stdDev, 0, str);
      xmlNewChild(node, NULL, BAD_CAST "standardDeviation", BAD_CAST str);
      sprintf(str, "%d", quality->imageDataQuality[ii].missingLines);
      xmlNewChild(node, NULL, BAD_CAST "missingLines", BAD_CAST str);
      double2str(quality->imageDataQuality[ii].bitErrorRate, 0, str);
      xmlNewChild(node, NULL, BAD_CAST "bitErrorRate", BAD_CAST str);
      double2str(quality->imageDataQuality[ii].noData, 0, str);
      xmlNewChild(node, NULL, BAD_CAST "noDataValue", BAD_CAST str);
    }
  }
  parent = xmlNewChild(section, NULL, BAD_CAST "limits", NULL);
  node = xmlNewChild(parent, NULL, BAD_CAST "rawData", NULL);
  sprintf(str, "%d", quality->gapDefinition);
  xmlNewChild(node, NULL, BAD_CAST "gapDefinition", BAD_CAST str);
  double2str(quality->gapPercentageLimit, 0, str);
  xmlNewChild(node, NULL, BAD_CAST "gapPercentageLimit", BAD_CAST str);
  double2str(quality->missingLinePercentageLimit, 0, str);
  xmlNewChild(node, NULL, BAD_CAST "missingLinePercentageLimit", BAD_CAST str);
  double2str(quality->bitErrorLimit, 0, str);
  xmlNewChild(node, NULL, BAD_CAST "bitErrorLimit", BAD_CAST str);
  double2str(quality->timeReconstructionPercentageLimit, 0, str);
  xmlNewChild(node, NULL, BAD_CAST "timeReconstructionPercentageLimit",
	      BAD_CAST str);
  node = xmlNewChild(parent, NULL, BAD_CAST "processing", NULL);
  double2str(quality->dopplerCentroidLimit, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "dopplerCentroidLimit", BAD_CAST str);
  xmlNewProp(unit, BAD_CAST "units", BAD_CAST "Hz");
  double2str(quality->geolocationQualityLimit, 0, str);
  unit = xmlNewChild(node, NULL, BAD_CAST "geolocationQualityLimit", 
		     BAD_CAST str);
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
  FREE(str);
}

void cornerCoords2boundingBox(iso_sceneCoord corner[4], 
			      double *westLon, double *eastLon,
			      double *southLat, double *northLat)
{
  int ii;
  double lat, lon, minLat=90, maxLat=-90, minLon=180, maxLon=-180;
  for (ii=0; ii<4; ii++) {
    lat = corner[ii].lat;
    lon = corner[ii].lon;
    if (lat > maxLat)
      maxLat = lat;
    if (lat < minLat)
      minLat = lat;
    if (lon > maxLon)
      maxLon = lon;
    if (lon < minLon)
      minLon = lon;
  }
  *westLon = minLon;
  *eastLon = maxLon;
  *southLat = minLat;
  *northLat = maxLat;
}

void iso_ext_meta_write(iso_meta *iso, const char *outFile, 
			const char *fileIdentifier, 
			const char *iso_meta_configFile,
			const char *gapFile)
{
  char *str = (char *) MALLOC(sizeof(char)*1024);
  int ii;
 
  // Check ISO metadata configuration file
  if (!fileExists(iso_meta_configFile))
    asfPrintError("ISO metadata configuration file (%s) does not exist!\n", 
		  iso_meta_configFile);
  xmlDoc *config = xmlReadFile(iso_meta_configFile, NULL, 0);
  if (!config)
    asfPrintError("Could not parse ISO metadata configuration file (%s)\n", 
		  iso_meta_configFile);  
  
  // Convenience pointers
  iso_generalHeader *header = iso->generalHeader;
  iso_productComponents *comps = iso->productComponents;
  iso_productInfo *info = iso->productInfo;
  iso_setup *setup = iso->setup;
  iso_productQuality *quality = iso->productQuality;
  iso_processing *proc = iso->processing;
  iso_instrument *inst = iso->instrument;
  iso_productSpecific *spec = iso->productSpecific;
  
  // Set up 
  xmlDoc *doc = xmlNewDoc(BAD_CAST "1.0");
  xmlNs *gmd = xmlNewNs(NULL, BAD_CAST "http://www.isotc211.org/2005/gmd", 
			BAD_CAST "gmd");
  xmlNode *root = xmlNewNode(gmd, BAD_CAST "DS_Series");
  xmlNs *xsi = xmlNewNs(root, 
			BAD_CAST "http://www.w3.org/2001/XMLSchema-instance",
			BAD_CAST "xsi");
  xmlNewNsProp(root, xsi, BAD_CAST "noNamespaceSchemaLocation",
  	       BAD_CAST "schema.xsd");
  //xmlNs *mgl = xmlNewNs(root, 
  //			BAD_CAST "http://www.isotc211.org/2005/mgl/1.0/2013", 
  //			BAD_CAST "mgl"); 
  gmd = xmlNewNs(root, BAD_CAST "http://www.isotc211.org/2005/gmd", 
		 BAD_CAST "gmd");
  xmlNs *gco = xmlNewNs(root, BAD_CAST "http://www.isotc211.org/2005/gco", 
			BAD_CAST "gco"); 
  //xmlNs *srv = xmlNewNs(root, BAD_CAST "http://www.isotc211.org/2005/srv", 
  //			BAD_CAST "srv");
  xmlNs *xs = xmlNewNs(root, BAD_CAST "http://www.w3.org/2001/XMLSchema", 
		       BAD_CAST "xs");
  //xmlNs *gsr = xmlNewNs(root, BAD_CAST "http://www.isotc211.org/2005/gsr", 
  //			BAD_CAST "gsr");
  //xmlNs *gss = xmlNewNs(root, BAD_CAST "http://www.isotc211.org/2005/gss", 
  //			BAD_CAST "gss");
  //xmlNs *gts = xmlNewNs(root, BAD_CAST "http://www.isotc211.org/2005/gts", 
  //			BAD_CAST "gts");
  //xmlNs *gmx = xmlNewNs(root, BAD_CAST "http://www.isotc211.org/2005/gmx", 
  //			BAD_CAST "gmx");
  xmlNs *eos = xmlNewNs(root, BAD_CAST "http://earthdata.nasa.gov/schema/eos", 
			BAD_CAST "eos");
  xmlNs *echo = xmlNewNs(root, 
			 BAD_CAST "http://www.echo.nasa.gov/ingest/schemas/operatations",
			 BAD_CAST "echo");
  xmlNs *xlink = xmlNewNs(root, BAD_CAST "http://www.w3.org/1999/xlink", 
			  BAD_CAST "xlink"); 
  xmlNs *gml = xmlNewNs(root, BAD_CAST "http://www.opengis.net/gml/3.2", 
			BAD_CAST "gml"); 
  xmlNs *gmi = xmlNewNs(root, BAD_CAST "http://www.isotc211.org/2005/gmi", 
			BAD_CAST "gmi");
  xmlDocSetRootElement(doc, root);
  
  xmlNodePtr parent, section, node, node2, node3, node4, node5, node6, node7;
  xmlNodePtr node8, node9, node10, node11, node12, node13, node14, node15;
  xmlNodePtr node16, node17;
  xmlNodePtr composedOf = xmlNewChild(root, gmd, BAD_CAST "composedOf", NULL);
  xmlNodePtr ds_dataSet = 
    xmlNewChild(composedOf, gmd, BAD_CAST "DS_DataSet", NULL);
  xmlNodePtr has = xmlNewChild(ds_dataSet, gmd, BAD_CAST "has", NULL);
  xmlNodePtr mi_metadata = xmlNewChild(has, gmi, BAD_CAST "MI_Metadata", NULL);
  
  // fileIdentifier
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "fileIdentifier", NULL);
  xmlNewChild(parent, gco, BAD_CAST "CharacterString", BAD_CAST fileIdentifier);
  
  // language
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "language", NULL);
  xmlNewChild(parent, gco, BAD_CAST "CharacterString", BAD_CAST "eng");
  
  // characterSet
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "characterSet", NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "MD_CharacterSetCode", 
			BAD_CAST "utf8");
  xmlNewProp(section, BAD_CAST "codeList", 
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_CharacterSetCode");
  xmlNewProp(section, BAD_CAST "codeListValue", BAD_CAST "utf8");
  
  // hierarchyLevel
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "hierarchyLevel", NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "MD_ScopeCode", 
			BAD_CAST "dataset");
  xmlNewProp(section, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_ScopeCode");
  xmlNewProp(section, BAD_CAST "codeListValue", BAD_CAST "dataset");
  
  // contact
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "contact", NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "CI_ResponsibleParty", NULL);
  node = xmlNewChild(section, gmd, BAD_CAST "organisationName", NULL);
  sprintf(str, "%s", xml_get_string_value(config, 
    "MI_Metadata.contact.CI_ResponsibleParty.organisationName"));  
  xmlNewChild(node, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node = xmlNewChild(section, gmd, BAD_CAST "contactInfo", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "CI_Contact", NULL);
  xmlNewProp(node2, BAD_CAST "id", BAD_CAST "ASFContactInfo");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "phone", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "CI_Telephone", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "voice", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.contactInfo.CI_Contact.phone.CI_Telephone.voice"));
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "facsimile", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.contactInfo.CI_Contact.phone.CI_Telephone.facsimile"));
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "address", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "CI_Address", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "deliveryPoint", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.contactInfo.CI_Contact.address.CI_Address.deliveryPoint"));
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "city", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.contactInfo.CI_Contact.address.CI_Address.city"));
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "administrativeArea", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.contactInfo.CI_Contact.address.CI_Address.administrativeArea"));
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "postalCode", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.contactInfo.CI_Contact.address.CI_Address.postalCode"));
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "country", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.contactInfo.CI_Contact.address.CI_Address.country"));
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "electronicMailAddress", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.contactInfo.CI_Contact.address.CI_Address.electronicMailAddress"));
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node = xmlNewChild(section, gmd, BAD_CAST "role", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.contact.CI_ResponsibleParty.role.CI_RoleCode"));
  node2 = xmlNewChild(node, gmd, BAD_CAST "CI_RoleCode", BAD_CAST str);
  sprintf(str, "%s", xml_get_string_attribute(config,
    "MI_Metadata.contact.CI_ResponsibleParty.role.CI_RoleCode.codeList"));
  xmlNewProp(node2, BAD_CAST "codeList", BAD_CAST str);
  sprintf(str, "%s", xml_get_string_attribute(config,
    "MI_Metadata.contact.CI_ResponsibleParty.role.CI_RoleCode.codeListValue"));
  xmlNewProp(node2, BAD_CAST "codeListValue", BAD_CAST str);

  // dateStamp
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "dateStamp", NULL);
  dateTime2str(header->generationTime, str);
  xmlNewChild(parent, gco, BAD_CAST "DateTime", BAD_CAST str);
  
  // metadataStandardName
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "metadataStandardName", NULL);
  xmlNewChild(parent, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ISO 19115-2 Geographic information — Metadata — Part 2: Extensions for imagery and gridded data");

  // metadataStandardVersion
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "metadataStandardVersion",
		       NULL);
  xmlNewChild(parent, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ISO 19115-2:2009-02-15");
  
  // spatialRepresentationInfo
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "spatialRepresentationInfo",
		       NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "MD_GridSpatialRepresentation", 
			NULL);

  // spatialRepresentationInfo - numberOfDimensions
  node = xmlNewChild(section, gmd, BAD_CAST "numberOfDimensions", NULL);
  xmlNewChild(node, gco, BAD_CAST "Integer", BAD_CAST "2");

  // spatialRepresentationInfo - axisDimensionProperties (track)
  node = xmlNewChild(section, gmd, BAD_CAST "axisDimensionProperties", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_Dimension", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "dimensionName", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "MD_DimensionNameTypeCode",
		      BAD_CAST "track");
  xmlNewProp(node4, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_DimensionNameTypeCode");
  xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "track");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "dimensionSize", NULL);
  sprintf(str, "%d", info->numberOfRows);
  xmlNewChild(node3, gco, BAD_CAST "Integer", BAD_CAST str);

  // spatialRepresentationInfo - axisDimensionProperties (crossTrack)
  node = xmlNewChild(section, gmd, BAD_CAST "axisDimensionProperties", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_Dimension", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "dimensionName", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "MD_DimensionNameTypeCode",
		      BAD_CAST "crossTrack");
  xmlNewProp(node4, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_DimensionNameTypeCode");
  xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "crossTrack");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "dimensionSize", NULL);
  sprintf(str, "%d", info->numberOfColumns);
  xmlNewChild(node3, gco, BAD_CAST "Integer", BAD_CAST str);

  // spatialRepresentationInfo - cellGeometry
  node = xmlNewChild(section, gmd, BAD_CAST "cellGeometry", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_CellGeometryCode", 
		      BAD_CAST "area");
  xmlNewProp(node2, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_CellGeometryCode");
  xmlNewProp(node2, BAD_CAST "codeListValue", BAD_CAST "area");

  // spatialRepresentationInfo - transformationParameterAvailability
  node = xmlNewChild(section, gmd, 
		     BAD_CAST "transformationParameterAvailability", NULL);
  xmlNewChild(node, gco, BAD_CAST "Boolean", BAD_CAST "false");

  node = xmlNewChild(section, eos, BAD_CAST "otherPropertyType", NULL);
  node2 = xmlNewChild(node, gco, BAD_CAST "RecordType", 
		      BAD_CAST "EOS Additional Attributes");
  xmlNewProp(node2, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
  node = xmlNewChild(section, eos, BAD_CAST "otherProperty", NULL);
  node2 = xmlNewChild(node, gco, BAD_CAST "Record", NULL);
  node3 = xmlNewChild(node2, eos, BAD_CAST "EOS_AdditionalAttributes", NULL);

  // spatialRepresentationInfo - extra parameters (startRow)
  node4 = xmlNewChild(node3, eos, BAD_CAST "additionalAttribute", NULL);
  node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node6 = xmlNewChild(node5, eos, BAD_CAST "reference", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttributeDescription",
		      NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "name", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", BAD_CAST "startRow");
  node8 = xmlNewChild(node7, eos, BAD_CAST "type", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributeTypeCode",
		      BAD_CAST "spatialInformation");
  xmlNewProp(node9, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node9, BAD_CAST "codeListValue", BAD_CAST "spatialInformation");
  xmlNewChild(node7, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node8 = xmlNewChild(node7, eos, BAD_CAST "description", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "start row of a subset");
  node8 = xmlNewChild(node7, eos, BAD_CAST "value", NULL);
  sprintf(str, "%i", info->startRow);
  xmlNewChild(node8, gco, BAD_CAST "Integer", BAD_CAST str);
    
  // spatialRepresentationInfo - extra parameters (startColumn)
  node4 = xmlNewChild(node3, eos, BAD_CAST "additionalAttribute", NULL);
  node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node6 = xmlNewChild(node5, eos, BAD_CAST "reference", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttributeDescription",
		      NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "name", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", BAD_CAST "startColumn");
  node8 = xmlNewChild(node7, eos, BAD_CAST "type", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributeTypeCode",
		      BAD_CAST "spatialInformation");
  xmlNewProp(node9, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node9, BAD_CAST "codeListValue", BAD_CAST "spatialInformation");
  xmlNewChild(node7, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node8 = xmlNewChild(node7, eos, BAD_CAST "description", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "start column of a subset");
  node8 = xmlNewChild(node7, eos, BAD_CAST "value", NULL);
  sprintf(str, "%i", info->startColumn);
  xmlNewChild(node8, gco, BAD_CAST "Integer", BAD_CAST str);
    
  // spatialRepresentationInfo - extra parameters (rowScaling)
  node4 = xmlNewChild(node3, eos, BAD_CAST "additionalAttribute", NULL);
  node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node6 = xmlNewChild(node5, eos, BAD_CAST "reference", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttributeDescription",
		      NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "name", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", BAD_CAST "rowScaling");
  node8 = xmlNewChild(node7, eos, BAD_CAST "type", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributeTypeCode",
		      BAD_CAST "spatialInformation");
  xmlNewProp(node9, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node9, BAD_CAST "codeListValue", BAD_CAST "spatialInformation");
  xmlNewChild(node7, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node8 = xmlNewChild(node7, eos, BAD_CAST "description", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "row scaling factor, in case the image has been resampled");
  node8 = xmlNewChild(node7, eos, BAD_CAST "value", NULL);
  double2str(info->rowScaling, 0, str);
  xmlNewChild(node8, gco, BAD_CAST "Real", BAD_CAST str);
    
  // spatialRepresentationInfo - extra parameters (columnScaling)
  node4 = xmlNewChild(node3, eos, BAD_CAST "additionalAttribute", NULL);
  node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node6 = xmlNewChild(node5, eos, BAD_CAST "reference", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttributeDescription",
		      NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "name", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", BAD_CAST "columnScaling");
  node8 = xmlNewChild(node7, eos, BAD_CAST "type", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributeTypeCode",
		      BAD_CAST "spatialInformation");
  xmlNewProp(node9, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node9, BAD_CAST "codeListValue", BAD_CAST "spatialInformation");
  xmlNewChild(node7, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node8 = xmlNewChild(node7, eos, BAD_CAST "description", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "column scaling factor, in case the image has been resampled");
  node8 = xmlNewChild(node7, eos, BAD_CAST "value", NULL);
  double2str(info->columnScaling, 0, str);
  xmlNewChild(node8, gco, BAD_CAST "Real", BAD_CAST str);
    
  // spatialRepresentationInfo - extra parameters (sceneSpecification)
  node4 = xmlNewChild(node3, eos, BAD_CAST "additionalAttribute", NULL);
  node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node6 = xmlNewChild(node5, eos, BAD_CAST "reference", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttributeDescription",
		      NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "name", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "sceneSpecification");
  node8 = xmlNewChild(node7, eos, BAD_CAST "type", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributeTypeCode",
		      BAD_CAST "spatialInformation");
  xmlNewProp(node9, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node9, BAD_CAST "codeListValue", BAD_CAST "spatialInformation");
  xmlNewChild(node7, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node8 = xmlNewChild(node7, eos, BAD_CAST "description", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "FRAME, TIME or CENTERCOORDS");
  node8 = xmlNewChild(node7, eos, BAD_CAST "value", NULL);
  if (setup->sceneSpecification == FRAME_SPEC)
    strcpy(str, "FRAME");
  else if (setup->sceneSpecification == TIME_SPEC)
    strcpy(str, "TIME");
  else if (setup->sceneSpecification == CENTERCOORDS_SPEC)
    strcpy(str, "CENTERCOORDS");
  else if (setup->sceneSpecification == UNDEF_SCENE_SPEC)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", BAD_CAST str);
    
  // spatialRepresentationInfo - extra parameters (sceneAverageHeight)
  node4 = xmlNewChild(node3, eos, BAD_CAST "additionalAttribute", NULL);
  node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node6 = xmlNewChild(node5, eos, BAD_CAST "reference", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttributeDescription",
		      NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "name", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "sceneAverageHeight");
  node8 = xmlNewChild(node7, eos, BAD_CAST "type", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributeTypeCode",
		      BAD_CAST "spatialInformation");
  xmlNewProp(node9, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node9, BAD_CAST "codeListValue", BAD_CAST "spatialInformation");
  xmlNewChild(node7, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node8 = xmlNewChild(node7, eos, BAD_CAST "description", NULL);
  xmlNewChild(node8, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "scene average height with respect to reference frame [m]");
  node8 = xmlNewChild(node7, eos, BAD_CAST "value", NULL);
  double2str(info->sceneAverageHeight, 3, str);
  xmlNewChild(node8, gco, BAD_CAST "Real", BAD_CAST str);
    
  // identificationInfo
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "identificationInfo", NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "MD_DataIdentification", NULL);

  // identificationInfo - citation
  node = xmlNewChild(section, gmd, BAD_CAST "citation", NULL); // mandatory

  // identificationInfo - abstract
  node = xmlNewChild(section, gmd, BAD_CAST "abstract", NULL); // mandatory

  // identificationInfo - status
  node = xmlNewChild(section, gmd, BAD_CAST "status", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.identificationInfo.MD_DataIdentification.status.MD_ProgressCode"));
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_ProgressCode", BAD_CAST str);
  sprintf(str, "%s", xml_get_string_attribute(config,
    "MI_Metadata.identificationInfo.MD_DataIdentification.status.MD_ProgressCode.codeList"));
  xmlNewProp(node2, BAD_CAST "codeList", BAD_CAST str);
  sprintf(str, "%s", xml_get_string_attribute(config,
    "MI_Metadata.identificationInfo.MD_DataIdentification.status.MD_ProgressCode.codeListValue"));
  xmlNewProp(node2, BAD_CAST "codeListValue", BAD_CAST str);

  // identificationInfo - browseImage
  node = xmlNewChild(section, gmd, BAD_CAST "graphicOverview", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_BrowseGraphic", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "fileName", NULL);
  if (strlen(comps->browseImage.host) > 1 && 
      strlen(comps->browseImage.path) > 1)
    sprintf(str, "%s%c%s%c%s", comps->browseImage.host, DIR_SEPARATOR,
	    comps->browseImage.path, DIR_SEPARATOR, comps->browseImage.name);
  else if (strlen(comps->browseImage.path) > 1)
    sprintf(str, "%s%c%s", comps->browseImage.path, DIR_SEPARATOR,
	    comps->browseImage.name);
  else
    sprintf(str, "%s", comps->browseImage.name);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "fileType", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST info->browseImageDataFormat);

  // identificationInfo - resourceConstraints
  node = xmlNewChild(section, gmd, BAD_CAST "resourceConstraints", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_LegalConstraints", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "useLimitation", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.identificationInfo.MD_DataIdentification.resourceConstraints.MD_LegalConstraints.useLimitation"));
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "accessConstraints", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "MD_RestrictionCode", 
		      BAD_CAST "copyright");
  xmlNewProp(node4, BAD_CAST "codeList", 
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_RestrictionCode");
  xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "copyright");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "useConstraints", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "MD_RestrictionCode", 
		      BAD_CAST "copyright");
  xmlNewProp(node4, BAD_CAST "codeList", 
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_RestrictionCode");
  xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "copyright");
  node = xmlNewChild(section, gmd, BAD_CAST "resourceConstraints", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_SecurityConstraints", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "useLimitation", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.identificationInfo.MD_DataIdentification.resourceConstraints.MD_SecurityConstraints.useLimitation"));
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "classification", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "MD_ClassificationCode", 
		      BAD_CAST "unclassified");
  xmlNewProp(node4, BAD_CAST "codeList", 
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_ClassificationCode");
  xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "unclassified");  

  // identificationInfo - aggregationInfo (annotation file)
  if (comps->annotation) {
    for (ii=0; ii<comps->numAnnotations; ii++) {
      node = xmlNewChild(section, gmd, BAD_CAST "aggregationInfo", NULL);
      node2 = xmlNewChild(node, gmd, BAD_CAST "MD_AggregateInformation", NULL);
      node3 = xmlNewChild(node2, gmd, BAD_CAST "aggregateDataSetName", NULL);
      node4 = xmlNewChild(node3, gmd, BAD_CAST "CI_Citation", NULL);
      if (strlen(comps->annotation[ii].file.host) > 1 && 
	  strlen(comps->annotation[ii].file.path) > 1)
	sprintf(str, "%s%c%s%c%s", comps->annotation[ii].file.host, 
		DIR_SEPARATOR, comps->annotation[ii].file.path, 
		DIR_SEPARATOR, comps->annotation[ii].file.name);
      else if (strlen(comps->annotation[ii].file.path) > 1)
	sprintf(str, "%s%c%s", comps->annotation[ii].file.path, 
		DIR_SEPARATOR, comps->annotation[ii].file.name);
      else
	sprintf(str, "%s", comps->annotation[ii].file.name);
      node5 = xmlNewChild(node4, gmd, BAD_CAST "title", NULL);
      xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
      xmlNewChild(node4, gmd, BAD_CAST "date", NULL);
      node5 = xmlNewChild(node4, gmd, BAD_CAST "identifier", NULL);
      node6 = xmlNewChild(node5, gmd, BAD_CAST "MD_Identifier", NULL);
      node7 = xmlNewChild(node6, gmd, BAD_CAST "code", NULL);
      xmlNewChild(node7, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "annotation file");
      node5 = xmlNewChild(node4, gmd, BAD_CAST "presentationForm", NULL);
      node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_PresentationFormCode",
			  BAD_CAST "documentDigital");
      xmlNewProp(node6, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_PresentationFormCode");
      xmlNewProp(node6, BAD_CAST "codeListValue", BAD_CAST "documentDigital");
      node3 = xmlNewChild(node2, gmd, BAD_CAST "associationType", NULL);
      node4 = xmlNewChild(node3, gmd, BAD_CAST "DS_AssociationTypeCode",
			  BAD_CAST "crossReference");
      xmlNewProp(node4, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#DS_AssociationTypeCode");
      xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "crossReference");
    }
  }

  // identificationInfo - aggregationInfo (image data file)
  if (comps->imageData) {
    for (ii=0; ii<comps->numLayers; ii++) {
      node = xmlNewChild(section, gmd, BAD_CAST "aggregationInfo", NULL);
      node2 = xmlNewChild(node, gmd, BAD_CAST "MD_AggregateInformation", NULL);
      node3 = xmlNewChild(node2, gmd, BAD_CAST "aggregateDataSetName", NULL);
      node4 = xmlNewChild(node3, gmd, BAD_CAST "CI_Citation", NULL);
      if (strlen(comps->imageData[ii].file.host) > 1 && 
	  strlen(comps->imageData[ii].file.path) > 1)
	sprintf(str, "%s%c%s%c%s", comps->imageData[ii].file.host, 
		DIR_SEPARATOR, comps->imageData[ii].file.path, DIR_SEPARATOR, 
		comps->imageData[ii].file.name);
      else if (strlen(comps->imageData[ii].file.path) > 1)
	sprintf(str, "%s%c%s", comps->imageData[ii].file.path, DIR_SEPARATOR,
		comps->imageData[ii].file.name);
      else
	sprintf(str, "%s", comps->imageData[ii].file.name);
      node5 = xmlNewChild(node4, gmd, BAD_CAST "title", NULL);
      xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
      xmlNewChild(node4, gmd, BAD_CAST "date", NULL);
      node5 = xmlNewChild(node4, gmd, BAD_CAST "identifier", NULL);
      node6 = xmlNewChild(node5, gmd, BAD_CAST "MD_Identifier", NULL);
      node7 = xmlNewChild(node6, gmd, BAD_CAST "code", NULL);
      xmlNewChild(node7, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "image data file");
      node5 = xmlNewChild(node4, gmd, BAD_CAST "presentationForm", NULL);
      node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_PresentationFormCode",
			  BAD_CAST "imageDigital");
      xmlNewProp(node6, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_PresentationFormCode");
      xmlNewProp(node6, BAD_CAST "codeListValue", BAD_CAST "imageDigital");

      node3 = xmlNewChild(node2, gmd, BAD_CAST "associationType", NULL);
      node4 = xmlNewChild(node3, gmd, BAD_CAST "DS_AssociationTypeCode",
			  BAD_CAST "crossReference");
      xmlNewProp(node4, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#DS_AssociationTypeCode");
      xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "crossReference");

      node3 = xmlNewChild(node2, eos, BAD_CAST "otherPropertyType", NULL);
      node4 = xmlNewChild(node3, gco, BAD_CAST "RecordType", 
			   BAD_CAST "EOS Additional Attributes");
      xmlNewProp(node4, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
      node3 = xmlNewChild(node2, eos, BAD_CAST "otherProperty", NULL);
      node4 = xmlNewChild(node3, gco, BAD_CAST "Record", NULL);

      // identificationInfo - imageData (dataStartWith)
      node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttributes", 
			  NULL);
      node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
      node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
      node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributeDescription",
			  NULL);
      node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "dataStartWith");
      node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
      node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttributeTypeCode",
			  BAD_CAST "spatialInformation");
      xmlNewProp(node11, BAD_CAST "codeList", 
		 BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
      xmlNewProp(node11, BAD_CAST "codeListValue", 
		 BAD_CAST "spatialInformation");
      xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
      node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "EARLYAZNEARRG, EARLYAZFARRG, LATEAZNEARRG or LATEAZFARRG");
      node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
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
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST str);
    }
  }

  // identificationInfo - aggregationInfo (quicklooks)
  if (comps->quicklooks) {
    for (ii=0; ii<comps->numLayers; ii++) {
      node = xmlNewChild(section, gmd, BAD_CAST "aggregationInfo", NULL);
      node2 = xmlNewChild(node, gmd, BAD_CAST "MD_AggregateInformation", NULL);
      node3 = xmlNewChild(node2, gmd, BAD_CAST "aggregateDataSetName", NULL);
      node4 = xmlNewChild(node3, gmd, BAD_CAST "CI_Citation", NULL);
      if (strlen(comps->quicklooks[ii].file.host) > 1 && 
	  strlen(comps->quicklooks[ii].file.path) > 1)
	sprintf(str, "%s%c%s%c%s", comps->quicklooks[ii].file.host, 
		DIR_SEPARATOR, comps->quicklooks[ii].file.path, DIR_SEPARATOR, 
		comps->quicklooks[ii].file.name);
      else if (strlen(comps->quicklooks[ii].file.path) > 1)
	sprintf(str, "%s%c%s", comps->quicklooks[ii].file.path, DIR_SEPARATOR,
		comps->quicklooks[ii].file.name);
      else
	sprintf(str, "%s", comps->quicklooks[ii].file.name);
      node5 = xmlNewChild(node4, gmd, BAD_CAST "title", NULL);
      xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
      xmlNewChild(node4, gmd, BAD_CAST "date", NULL);
      node5 = xmlNewChild(node4, gmd, BAD_CAST "identifier", NULL);
      node6 = xmlNewChild(node5, gmd, BAD_CAST "MD_Identifier", NULL);
      node7 = xmlNewChild(node6, gmd, BAD_CAST "code", NULL);
      xmlNewChild(node7, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "quicklook file");
      node5 = xmlNewChild(node4, gmd, BAD_CAST "presentationForm", NULL);
      node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_PresentationFormCode",
			  BAD_CAST "imageDigital");
      xmlNewProp(node6, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_PresentationFormCode");
      xmlNewProp(node6, BAD_CAST "codeListValue", BAD_CAST "imageDigital");

      node3 = xmlNewChild(node2, gmd, BAD_CAST "associationType", NULL);
      node4 = xmlNewChild(node3, gmd, BAD_CAST "DS_AssociationTypeCode",
			  BAD_CAST "crossReference");
      xmlNewProp(node4, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#DS_AssociationTypeCode");
      xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "crossReference");

      node3 = xmlNewChild(node2, eos, BAD_CAST "otherPropertyType", NULL);
      node4 = xmlNewChild(node3, gco, BAD_CAST "RecordType", 
			  BAD_CAST "EOS Additional Attributes");
      xmlNewProp(node4, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
      node3 = xmlNewChild(node2, eos, BAD_CAST "otherProperty", NULL);
      node4 = xmlNewChild(node3, gco, BAD_CAST "Record", NULL);
      node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttributes", 
			  NULL);
      
      // identificationInfo - quicklook image (imageDataFormat)
      node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
      node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
      node9 = xmlNewChild(node8, eos, 
			  BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
      node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "imageDataFormat");
      node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
      node11 = xmlNewChild(node10, eos, 
			  BAD_CAST "EOS_AdditionalAttributeTypeCode",
			  BAD_CAST "imageInformation");
      xmlNewProp(node11, BAD_CAST "codeList", 
		 BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
      xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
      xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
      node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "image data format (TIFF greyscale)");
      node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST info->quicklooks.imageDataFormat);
      
      // identificationInfo - quicklook image (imageDataDepth)
      node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
      node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
      node9 = xmlNewChild(node8, eos, 
			  BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
      node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "imageDataDepth");
      node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
      node11 = xmlNewChild(node10, eos, 
			  BAD_CAST "EOS_AdditionalAttributeTypeCode",
			  BAD_CAST "imageInformation");
      xmlNewProp(node11, BAD_CAST "codeList", 
		 BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
      xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
      xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
      node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "image data depth (16 bit)");
      node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
      sprintf(str, "%d", info->quicklooks.imageDataDepth);      
      xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);
      
      // identificationInfo - quicklook image (numberOfRows)
      node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
      node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
      node9 = xmlNewChild(node8, eos, 
			  BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
      node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "numberOfRows");
      node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
      node11 = xmlNewChild(node10, eos, 
			  BAD_CAST "EOS_AdditionalAttributeTypeCode",
			  BAD_CAST "imageInformation");
      xmlNewProp(node11, BAD_CAST "codeList", 
		 BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
      xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
      xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
      node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "line count");
      node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
      sprintf(str, "%d", info->quicklooks.numberOfRows);      
      xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);
      
      // identificationInfo - quicklook image (numberOfColumns)
      node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
      node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
      node9 = xmlNewChild(node8, eos, 
			  BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
      node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "numberOfColumns");
      node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
      node11 = xmlNewChild(node10, eos, 
			  BAD_CAST "EOS_AdditionalAttributeTypeCode",
			  BAD_CAST "imageInformation");
      xmlNewProp(node11, BAD_CAST "codeList", 
		 BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
      xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
      xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
      node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "sample count");
      node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
      sprintf(str, "%d", info->quicklooks.numberOfColumns);      
      xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);
      
      // identificationInfo - quicklook image (columnBlockLength)
      node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
      node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
      node9 = xmlNewChild(node8, eos, 
			  BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
      node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "columnBlockLength");
      node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
      node11 = xmlNewChild(node10, eos, 
			  BAD_CAST "EOS_AdditionalAttributeTypeCode",
			  BAD_CAST "imageInformation");
      xmlNewProp(node11, BAD_CAST "codeList", 
		 BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
      xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
      xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
      node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "resampling factor in range [pixels]");
      node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", 
			   NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "pixels");
      node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
      double2str(info->quicklooks.columnBlockLength, 0, str);
      xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
      
      // identificationInfo - quicklook image (rowBlockLength)
      node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
      node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
      node9 = xmlNewChild(node8, eos, 
			  BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
      node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "rowBlockLength");
      node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
      node11 = xmlNewChild(node10, eos, 
			  BAD_CAST "EOS_AdditionalAttributeTypeCode",
			  BAD_CAST "imageInformation");
      xmlNewProp(node11, BAD_CAST "codeList", 
		 BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
      xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
      xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
      node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "resampling factor in azimuth [pixels]");
      node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", 
			   NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "pixels");
      node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
      double2str(info->quicklooks.rowBlockLength, 0, str);
      xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
      // identificationInfo - quicklook image (dataStartWith)
      node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
      node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
      node9 = xmlNewChild(node8, eos, 
			  BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
      node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "dataStartWith");
      node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
      node11 = xmlNewChild(node10, eos, 
			  BAD_CAST "EOS_AdditionalAttributeTypeCode",
			  BAD_CAST "imageInformation");
      xmlNewProp(node11, BAD_CAST "codeList", 
		 BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
      xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
      xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
      node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		  BAD_CAST "EARLYAZNEARRG, EARLYAZFARRG, LATEAZNEARRG or LATEAZFARRG");
      node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
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
      xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST str);
    }
  }

  // identificationInfo - aggregationInfo (map plot file)
  node = xmlNewChild(section, gmd, BAD_CAST "aggregationInfo", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_AggregateInformation", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "aggregateDataSetName", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "CI_Citation", NULL);
  if (strlen(comps->mapPlot.host) > 1 && strlen(comps->mapPlot.path) > 1)
    sprintf(str, "%s%c%s%c%s", comps->imageData[ii].file.host, DIR_SEPARATOR, 
	    comps->mapPlot.path, DIR_SEPARATOR, comps->mapPlot.name);
  else if (strlen(comps->mapPlot.path) > 1)
    sprintf(str, "%s%c%s", comps->mapPlot.path, DIR_SEPARATOR, 
	    comps->mapPlot.name);
  else
    sprintf(str, "%s", comps->mapPlot.name);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  xmlNewChild(node4, gmd, BAD_CAST "date", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "identifier", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "MD_Identifier", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "code", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "map plot");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "presentationForm", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_PresentationFormCode",
		      BAD_CAST "mapDigital");
  xmlNewProp(node6, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_PresentationFormCode");
  xmlNewProp(node6, BAD_CAST "codeListValue", BAD_CAST "mapDigital");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "associationType", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DS_AssociationTypeCode",
		      BAD_CAST "crossReference");
  xmlNewProp(node4, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#DS_AssociationTypeCode");
  xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "crossReference");
  
  // identificationInfo - spatialRepresentationType
  node = xmlNewChild(section, gmd, BAD_CAST "spatialRepresentationType", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_SpatialRepresentationTypeCode", 
		      BAD_CAST "grid");
  xmlNewProp(node2, BAD_CAST "codeList", 
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_SpatialRepresentationTypeCode");
  xmlNewProp(node2, BAD_CAST "codeListValue", BAD_CAST "grid");

  // identificationInfo - spatialResolution
  node = xmlNewChild(section, gmd, BAD_CAST "spatialResolution", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_Resolution", NULL);
  sprintf(str, "%.3lf", info->groundRangeResolution);
  node3 = xmlNewChild(node2, gco, BAD_CAST "Distance", BAD_CAST str);
  xmlNewProp(node3, BAD_CAST "uom", BAD_CAST "meter");
  xmlNewProp(node3, BAD_CAST "dimension", BAD_CAST "range");
  sprintf(str, "%.3lf", info->azimuthResolution);
  node3 = xmlNewChild(node2, gco, BAD_CAST "Distance", BAD_CAST str);
  xmlNewProp(node3, BAD_CAST "uom", BAD_CAST "meter");
  xmlNewProp(node3, BAD_CAST "dimension", BAD_CAST "azimuth");

  // identificationInfo - language
  node = xmlNewChild(section, gmd, BAD_CAST "language", NULL);
  xmlNewChild(node, gco, BAD_CAST "CharacterString", BAD_CAST "eng");

  // identificationInfo - characterSet
  node = xmlNewChild(section, gmd, BAD_CAST "characterSet", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_CharacterSetCode", 
		      BAD_CAST "utf8");
  xmlNewProp(node2, BAD_CAST "codeList", 
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_CharacterSetCode");
  xmlNewProp(node2, BAD_CAST "codeListValue", BAD_CAST "utf8");

  // identificationInfo - topicCategory
  node = xmlNewChild(section, gmd, BAD_CAST "topicCategory", NULL);
  xmlNewChild(node, gmd, BAD_CAST "MD_TopicCategoryCode", 
	      BAD_CAST "geoscientificInformation");

  // identificationInfo - environmentDescription
  node = xmlNewChild(section, gmd, BAD_CAST "environmentDescription", NULL);
  xmlNewChild(node, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Data product generated in HDF5 format with ISO 19115 conformant metadata.");

  // identificationInfo - extent
  node = xmlNewChild(section, gmd, BAD_CAST "extent", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "EX_Extent", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "description", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "geographic and temporal extent of data frame");

  node3 = xmlNewChild(node2, gmd, BAD_CAST "EX_BoundingPolygon", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "extentTypeCode", NULL);
  xmlNewChild(node4, gco, BAD_CAST "Boolean", BAD_CAST "true");
  node4 = xmlNewChild(node3, gmd, BAD_CAST "polygon", NULL);
  xmlNewProp(node4, BAD_CAST "srsDimension", BAD_CAST "2");
  xmlNewProp(node4, BAD_CAST "srsName", BAD_CAST "EPSG:4326");
  for (ii=0; ii<4; ii++) {
    node5 = xmlNewChild(node4, gml, BAD_CAST "Point", NULL);
    sprintf(str, "%.5lf %.5lf", 
	    info->sceneCornerCoord[ii].lon, info->sceneCornerCoord[ii].lat);
    xmlNewChild(node5, gml, BAD_CAST "pos", BAD_CAST str);
    node6 = xmlNewChild(node5, eos, BAD_CAST "otherPropertyType", NULL);
    node7 = xmlNewChild(node6, gco, BAD_CAST "RecordType", 
			BAD_CAST "EOS Additional Attributes");
    xmlNewProp(node7, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
    node6 = xmlNewChild(node5, eos, BAD_CAST "otherProperty", NULL);
    node7 = xmlNewChild(node6, gco, BAD_CAST "Record", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "EOS_AdditionalAttributes", NULL);
    
    // identificationInfo - extra parameters (refRow)
    node9 = xmlNewChild(node8, eos, BAD_CAST "additionalAttribute", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node11 = xmlNewChild(node10, eos, BAD_CAST "reference", NULL);
    node12 = xmlNewChild(node11, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node13 = xmlNewChild(node12, eos, BAD_CAST "name", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", BAD_CAST "refRow");
    node13 = xmlNewChild(node12, eos, BAD_CAST "type", NULL);
    node14 = xmlNewChild(node13, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node14, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node14, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node12, eos, BAD_CAST "dataType", BAD_CAST "INT");
    node13 = xmlNewChild(node12, eos, BAD_CAST "description", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", 
		BAD_CAST "position in image: row");
    node13 = xmlNewChild(node12, eos, BAD_CAST "value", NULL);
    sprintf(str, "%i", info->sceneCornerCoord[ii].refRow);
    xmlNewChild(node13, gco, BAD_CAST "Integer", BAD_CAST str);

    // identificationInfo - extra parameters (refColumn)
    node9 = xmlNewChild(node8, eos, BAD_CAST "additionalAttribute", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node11 = xmlNewChild(node10, eos, BAD_CAST "reference", NULL);
    node12 = xmlNewChild(node11, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node13 = xmlNewChild(node12, eos, BAD_CAST "name", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", BAD_CAST "refColumn");
    node13 = xmlNewChild(node12, eos, BAD_CAST "type", NULL);
    node14 = xmlNewChild(node13, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node14, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node14, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node12, eos, BAD_CAST "dataType", BAD_CAST "INT");
    node13 = xmlNewChild(node12, eos, BAD_CAST "description", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", 
		BAD_CAST "position in image: column");
    node13 = xmlNewChild(node12, eos, BAD_CAST "value", NULL);
    sprintf(str, "%i", info->sceneCornerCoord[ii].refColumn);
    xmlNewChild(node13, gco, BAD_CAST "Integer", BAD_CAST str);

    // identificationInfo - extra parameters (azimuthTimeUTC)
    node9 = xmlNewChild(node8, eos, BAD_CAST "additionalAttribute", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node11 = xmlNewChild(node10, eos, BAD_CAST "reference", NULL);
    node12 = xmlNewChild(node11, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node13 = xmlNewChild(node12, eos, BAD_CAST "name", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", 
		BAD_CAST "azimuthTimeUTC");
    node13 = xmlNewChild(node12, eos, BAD_CAST "type", NULL);
    node14 = xmlNewChild(node13, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node14, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node14, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node12, eos, BAD_CAST "dataType", BAD_CAST "DATETIME");
    node13 = xmlNewChild(node12, eos, BAD_CAST "description", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", 
		BAD_CAST "time stamp in UTC for coordinate");
    node13 = xmlNewChild(node12, eos, BAD_CAST "value", NULL);
    dateTime2str(info->sceneCornerCoord[ii].azimuthTimeUTC, str);
    xmlNewChild(node13, gco, BAD_CAST "DateTime", BAD_CAST str);

    // identificationInfo - extra parameters (rangeTime)
    node9 = xmlNewChild(node8, eos, BAD_CAST "additionalAttribute", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node11 = xmlNewChild(node10, eos, BAD_CAST "reference", NULL);
    node12 = xmlNewChild(node11, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node13 = xmlNewChild(node12, eos, BAD_CAST "name", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", BAD_CAST "rangeTime");
    node13 = xmlNewChild(node12, eos, BAD_CAST "type", NULL);
    node14 = xmlNewChild(node13, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node14, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node14, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node12, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node13 = xmlNewChild(node12, eos, BAD_CAST "description", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", 
		BAD_CAST "range time [s]");
    node13 = xmlNewChild(node12, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", BAD_CAST "s");
    node13 = xmlNewChild(node12, eos, BAD_CAST "value", NULL);
    double2str(info->sceneCornerCoord[ii].rangeTime, 0, str);
    xmlNewChild(node13, gco, BAD_CAST "Real", BAD_CAST str);

    // identificationInfo - extra parameters (incidenceAngle)
    node9 = xmlNewChild(node8, eos, BAD_CAST "additionalAttribute", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node11 = xmlNewChild(node10, eos, BAD_CAST "reference", NULL);
    node12 = xmlNewChild(node11, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node13 = xmlNewChild(node12, eos, BAD_CAST "name", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", 
		BAD_CAST "incidenceAngle");
    node13 = xmlNewChild(node12, eos, BAD_CAST "type", NULL);
    node14 = xmlNewChild(node13, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node14, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node14, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node12, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node13 = xmlNewChild(node12, eos, BAD_CAST "description", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", 
		BAD_CAST "incidence angle [degrees]");
    node13 = xmlNewChild(node12, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node13, gco, BAD_CAST "CharacterString", BAD_CAST "degrees");
    node13 = xmlNewChild(node12, eos, BAD_CAST "value", NULL);
    double2str(info->sceneCornerCoord[ii].incidenceAngle, 5, str);
    xmlNewChild(node13, gco, BAD_CAST "Real", BAD_CAST str);
  }

  node3 = xmlNewChild(node2, gmd, BAD_CAST "EX_TemporalExtent", NULL);
  xmlNewProp(node3, BAD_CAST "id", BAD_CAST "frameTemporalExtent");
  node4 = xmlNewChild(node3, gmd, BAD_CAST "extent", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "TimePeriod", NULL);
  dateTime2str(info->startTimeUTC, str);
  xmlNewChild(node5, gml, BAD_CAST "beginPosition", BAD_CAST str);
  dateTime2str(info->stopTimeUTC, str);
  xmlNewChild(node5, gml, BAD_CAST "endPosition", BAD_CAST str);

  // contentInfo
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "contentInfo", NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "MD_CoverageDescription", NULL);
  node = xmlNewChild(section, gmd, BAD_CAST "attributeDescription", NULL);
  xmlNewChild(node, gco, BAD_CAST "recordType", BAD_CAST "radar brightness");
  node = xmlNewChild(section, gmd, BAD_CAST "contentType", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_CoverageContentTypeCode", 
		      BAD_CAST "image");
  xmlNewProp(node2, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_CoverageContentTypeCode");
  xmlNewProp(node2, BAD_CAST "codeListValue", BAD_CAST "image");
  for (ii=0; ii<info->numberOfLayers; ii++) {
    node = xmlNewChild(section, gmd, BAD_CAST "dimension", NULL);
    node2 = xmlNewChild(node, gmi, BAD_CAST "MI_Band", NULL); 
    node3 = xmlNewChild(node2, gmd, BAD_CAST "maxValue", NULL);
    double2str(quality->imageDataQuality[ii].max, 0, str); 
    xmlNewChild(node3, gco, BAD_CAST "Real", BAD_CAST str);
    node3 = xmlNewChild(node2, gmd, BAD_CAST "minValue", NULL);
    double2str(quality->imageDataQuality[ii].min, 0, str);
    xmlNewChild(node3, gco, BAD_CAST "Real", BAD_CAST str);
    node3 = xmlNewChild(node2, gmd, BAD_CAST "bitsPerValue", NULL);
    sprintf(str, "%d", info->imageDataDepth);
    xmlNewChild(node3, gco, BAD_CAST "Integer", BAD_CAST str);
    node3 = xmlNewChild(node2, gmd, BAD_CAST "meanValue", NULL);
    double2str(quality->imageDataQuality[ii].mean, 0, str); 
    xmlNewChild(node3, gco, BAD_CAST "Real", BAD_CAST str);
    node3 = xmlNewChild(node2, gmd, BAD_CAST "standardDeviation", NULL);
    double2str(quality->imageDataQuality[ii].stdDev, 0, str);
    xmlNewChild(node3, gco, BAD_CAST "Real", BAD_CAST str);
    if (quality->imageDataQuality[ii].polLayer == HH_POL) {
      node3 = xmlNewChild(node2, gmi, BAD_CAST "transmittedPolarisation",
			  BAD_CAST "horizontal");
      xmlNewProp(node3, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MI_PolarizationOrientationCode");
      xmlNewProp(node3, BAD_CAST "codeListValue", BAD_CAST "horizontal");
      node3 = xmlNewChild(node2, gmi, BAD_CAST "detectedPolarisation",
			  BAD_CAST "horizontal");
      xmlNewProp(node3, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MI_PolarizationOrientationCode");
      xmlNewProp(node3, BAD_CAST "codeListValue", BAD_CAST "horizontal");
    }
    else if (quality->imageDataQuality[ii].polLayer == HV_POL) {
      node3 = xmlNewChild(node2, gmi, BAD_CAST "transmittedPolarisation",
			  BAD_CAST "horizontal");
      xmlNewProp(node3, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MI_PolarizationOrientationCode");
      xmlNewProp(node3, BAD_CAST "codeListValue", BAD_CAST "horizontal");
      node3 = xmlNewChild(node2, gmi, BAD_CAST "detectedPolarisation",
			  BAD_CAST "vertical");
      xmlNewProp(node3, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MI_PolarizationOrientationCode");
      xmlNewProp(node3, BAD_CAST "codeListValue", BAD_CAST "vertical");
    }
    else if (quality->imageDataQuality[ii].polLayer == VH_POL) {
      node3 = xmlNewChild(node2, gmi, BAD_CAST "transmittedPolarisation",
			  BAD_CAST "vertical");
      xmlNewProp(node3, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MI_PolarizationOrientationCode");
      xmlNewProp(node3, BAD_CAST "codeListValue", BAD_CAST "vertical");
      node3 = xmlNewChild(node2, gmi, BAD_CAST "detectedPolarisation",
			  BAD_CAST "horizontal");
      xmlNewProp(node3, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MI_PolarizationOrientationCode");
      xmlNewProp(node3, BAD_CAST "codeListValue", BAD_CAST "horizontal");
    }
    else if (quality->imageDataQuality[ii].polLayer == VV_POL) {
      node3 = xmlNewChild(node2, gmi, BAD_CAST "transmittedPolarisation",
			  BAD_CAST "vertical");
      xmlNewProp(node3, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MI_PolarizationOrientationCode");
      xmlNewProp(node3, BAD_CAST "codeListValue", BAD_CAST "vertical");
      node3 = xmlNewChild(node2, gmi, BAD_CAST "detectedPolarisation",
			  BAD_CAST "vertical");
      xmlNewProp(node3, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MI_PolarizationOrientationCode");
      xmlNewProp(node3, BAD_CAST "codeListValue", BAD_CAST "vertical");
    }
    node3 = xmlNewChild(node2, eos, BAD_CAST "otherPropertyType", NULL);
    node4 = xmlNewChild(node3, gco, BAD_CAST "RecordType", 
			BAD_CAST "EOS Additional Attributes");
    xmlNewProp(node4, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
    node3 = xmlNewChild(node2, eos, BAD_CAST "otherProperty", NULL);
    node4 = xmlNewChild(node3, gco, BAD_CAST "Record", NULL);
    node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttributes", NULL);
    
    // contentInfo - extra parameters (missingLines)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "missingLines");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "missing lines");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    sprintf(str, "%d", quality->imageDataQuality[ii].missingLines);
    xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);
    
    // contentInfo - extra parameters (bitErrorRate)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "bitErrorRate");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "bit error rate");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(quality->imageDataQuality[ii].bitErrorRate, 0, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (noDataValue)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "noDataValue");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "no data value");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(quality->imageDataQuality[ii].noData, 0, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (rowContent)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "rowContent");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "rangelines, northing (for geocoded products), x");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST info->rowContent);
    
    // contentInfo - extra parameters (columnContent)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "columnContent");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "azimuthlines, easting (for geocoded products), y");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST info->columnContent);
    
    // contentInfo - extra parameters (rowSpacing)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "rowSpacing");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "spacing of samples within a row from common raster [s or m]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "s");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(info->rowSpacing, 0, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (columnSpacing)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "columnSpacing");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "spacing within a column [s or m]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "s");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(info->columnSpacing, 0, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (rangeTimeFirstPixel)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "rangeTimeFirstPixel");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "minimum range time of entire scene [s]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "s");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(info->rangeTimeFirstPixel, 0, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (rangeTimeLastPixel)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "rangeTimeLastPixel");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "maximum range time of entire scene [s]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "s");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(info->rangeTimeLastPixel, 0, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (projectedSpacingSlantRange)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "projectedSpacingSlantRange");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "slant range [m]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "m");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(spec->projectedSpacingSlantRange, 3, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

    // contentInfo - extra parameters (slantRangeShift)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "slantRangeShift");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "slant range shift [m]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "m");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(spec->slantRangeShift, 3, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (projectedSpacingGroundRangeNear)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "projectedSpacingGroundRangeNear");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "ground near range [m]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "m");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(spec->projectedSpacingGroundNearRange, 3, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (projectedSpacingGroundRangeFar)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "projectedSpacingGroundRangeFar");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "ground far range [m]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "m");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(spec->projectedSpacingGroundFarRange, 3, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (projectedSpacingAzimuth)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "projectedSpacingAzimuth");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "nominal projected pixel spacing on ground at scene center [m]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "m");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(spec->projectedSpacingAzimuth, 0, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
    
    // contentInfo - extra parameters (slantRangeResolution)
    node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
    node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
    node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
    node9 = xmlNewChild(node8, eos, 
			BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
    node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "slantRangeResolution");
    node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
    node11 = xmlNewChild(node10, eos, 
			 BAD_CAST "EOS_AdditionalAttributeTypeCode",
			 BAD_CAST "imageInformation");
    xmlNewProp(node11, BAD_CAST "codeList", 
	       BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
    xmlNewProp(node11, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
    xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
    node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
		BAD_CAST "slant range resolution [m]");
    node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
    xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "m");
    node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
    double2str(spec->slantRangeResolution, 0, str);
    xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);
  }

  // distributionInfo
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "distributionInfo", NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "MD_Distribution", NULL);

  // distributionInfo - distributionFormat
  node = xmlNewChild(section, gmd, BAD_CAST "distributionFormat", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_Format", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "name", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", BAD_CAST "HDF5");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "version", NULL);
  sprintf(str, "%d.%d.%d", H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // distributionInfo - distributor
  node = xmlNewChild(section, gmd, BAD_CAST "distributor", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_Distributor", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "distributorContact", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "CI_ResponsibleParty", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "organisationName", NULL);
  sprintf(str, "%s", xml_get_string_value(config, 
    "MI_Metadata.distributionInfo.MD_Distribution.distributorContact.CI_ResponsibleParty.organisationName"));  
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST str);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "contactInfo", NULL);
  xmlNewProp(node5, BAD_CAST "xlink:href", BAD_CAST "#ASFContactInfo");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "role", NULL);
  sprintf(str, "%s", xml_get_string_value(config,
    "MI_Metadata.distributionInfo.MD_Distribution.distributorContact.CI_ResponsibleParty.role.CI_RoleCode"));
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_RoleCode", BAD_CAST str);
  sprintf(str, "%s", xml_get_string_attribute(config,
    "MI_Metadata.distributionInfo.MD_Distribution.distributorContact.CI_ResponsibleParty.role.CI_RoleCode.codeList"));
  xmlNewProp(node6, BAD_CAST "codeList", BAD_CAST str);
  sprintf(str, "%s", xml_get_string_attribute(config,
    "MI_Metadata.distributionInfo.MD_Distribution.distributorContact.CI_ResponsibleParty.role.CI_RoleCode.codeListValue"));
  xmlNewProp(node6, BAD_CAST "codeListValue", BAD_CAST str);

  // dataQualityInfo
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "dataQualityInfo", NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "DQ_DataQuality", NULL);

  // dataQualityInfo - scope
  node = xmlNewChild(section, gmd, BAD_CAST "scope", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_Scope", NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "level", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "MD_ScopeCode", BAD_CAST "dataset");
  xmlNewProp(node4, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_ScopeCode");
  xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "dataset");

  // dataQualityInfo - overall quality inspection
  node = xmlNewChild(section, gmd, BAD_CAST "report", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_CompletenessOmission",
		      NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "measureDescription", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "overall quality inspection");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "evaluationMethodType", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_EvaluationMethodTypeCode",
		      BAD_CAST "directInternal");
  xmlNewProp(node4, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#DQ_EvaluationMethodTypeCode");
  xmlNewProp(node4, BAD_CAST "codeListValue", BAD_CAST "directInternal");

  node3 = xmlNewChild(node2, gmd, BAD_CAST "result", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_ConformanceResult", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "specification", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_Citation", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ASF Seasat Data Quality document");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "date", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_Date", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
  node9 = xmlNewChild(node8, gmd, BAD_CAST "dateType", NULL);
  node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_DateTypeCode", 
		       BAD_CAST "publication");
  xmlNewProp(node10, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
  xmlNewProp(node10, BAD_CAST "codeListValue", BAD_CAST "publication");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "edition", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "version 1.0");

  node5 = xmlNewChild(node4, gmd, BAD_CAST "explanation", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString",
	      BAD_CAST "Data passes the overall quality criteria");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "pass", NULL);
  if (info->qualityInspection == AUTO_APPROVED ||
      info->qualityInspection == OPERATOR_APPROVED)
    boolean2str(TRUE, str);
  else
    boolean2str(FALSE, str);
  xmlNewChild(node5, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - gap significance flag
  node = xmlNewChild(section, gmd, BAD_CAST "report", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_QuantitativeAttributeAccuracy",
		      NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "measureDescription", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "gap signficance flag");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "result", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_ConformanceResult", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "specification", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_Citation", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ASF Seasat Data Quality document");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "date", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_Date", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
  node9 = xmlNewChild(node8, gmd, BAD_CAST "dateType", NULL);
  node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_DateTypeCode", 
		       BAD_CAST "publication");
  xmlNewProp(node10, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
  xmlNewProp(node10, BAD_CAST "codeListValue", BAD_CAST "publication");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "edition", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "version 1.0");

  node5 = xmlNewChild(node4, gmd, BAD_CAST "explanation", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Data beyond gap tolerance level");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "pass", NULL);
  boolean2str(quality->rawDataQuality[ii].gapSignificanceFlag, str);
  xmlNewChild(node5, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - missing lines significance flag
  node = xmlNewChild(section, gmd, BAD_CAST "report", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_QuantitativeAttributeAccuracy",
		      NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "measureDescription", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "missing lines signficance flag");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "result", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_ConformanceResult", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "specification", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_Citation", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ASF Seasat Data Quality document");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "date", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_Date", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
  node9 = xmlNewChild(node8, gmd, BAD_CAST "dateType", NULL);
  node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_DateTypeCode", 
		       BAD_CAST "publication");
  xmlNewProp(node10, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
  xmlNewProp(node10, BAD_CAST "codeListValue", BAD_CAST "publication");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "edition", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "version 1.0");

  node5 = xmlNewChild(node4, gmd, BAD_CAST "explanation", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Data beyond missing lines tolerance level");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "pass", NULL);
  boolean2str(quality->rawDataQuality[ii].missingLinesSignificanceFlag, str);
  xmlNewChild(node5, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - bit error significance flag
  node = xmlNewChild(section, gmd, BAD_CAST "report", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_QuantitativeAttributeAccuracy",
		      NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "measureDescription", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "bit error signficance flag");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "result", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_ConformanceResult", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "specification", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_Citation", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ASF Seasat Data Quality document");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "date", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_Date", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
  node9 = xmlNewChild(node8, gmd, BAD_CAST "dateType", NULL);
  node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_DateTypeCode", 
		       BAD_CAST "publication");
  xmlNewProp(node10, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
  xmlNewProp(node10, BAD_CAST "codeListValue", BAD_CAST "publication");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "edition", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "version 1.0");

  node5 = xmlNewChild(node4, gmd, BAD_CAST "explanation", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Data beyond bit error tolerance level");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "pass", NULL);
  boolean2str(quality->rawDataQuality[ii].bitErrorSignificanceFlag, str);
  xmlNewChild(node5, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - time reconstruction significance flag
  node = xmlNewChild(section, gmd, BAD_CAST "report", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_QuantitativeAttributeAccuracy",
		      NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "measureDescription", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "time reconstruction signficance flag");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "result", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_ConformanceResult", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "specification", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_Citation", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ASF Seasat Data Quality document");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "date", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_Date", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
  node9 = xmlNewChild(node8, gmd, BAD_CAST "dateType", NULL);
  node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_DateTypeCode", 
		       BAD_CAST "publication");
  xmlNewProp(node10, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
  xmlNewProp(node10, BAD_CAST "codeListValue", BAD_CAST "publication");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "edition", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "version 1.0");

  node5 = xmlNewChild(node4, gmd, BAD_CAST "explanation", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Data beyond time reconstruction tolerance level");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "pass", NULL);
  boolean2str(quality->rawDataQuality[ii].timeReconstructionSignificanceFlag, 
	      str);
  xmlNewChild(node5, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - Doppler ambiguity not zero flag
  node = xmlNewChild(section, gmd, BAD_CAST "report", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_QuantitativeAttributeAccuracy",
		      NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "measureDescription", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Doppler ambiguity not zero flag");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "result", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_ConformanceResult", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "specification", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_Citation", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ASF Seasat Data Quality document");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "date", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_Date", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
  node9 = xmlNewChild(node8, gmd, BAD_CAST "dateType", NULL);
  node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_DateTypeCode", 
		       BAD_CAST "publication");
  xmlNewProp(node10, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
  xmlNewProp(node10, BAD_CAST "codeListValue", BAD_CAST "publication");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "edition", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "version 1.0");

  node5 = xmlNewChild(node4, gmd, BAD_CAST "explanation", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Data has Doppler ambiguities");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "pass", NULL);
  boolean2str(quality->dopplerAmbiguityNotZeroFlag, str);
  xmlNewChild(node5, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - Doppler outside limits flag
  node = xmlNewChild(section, gmd, BAD_CAST "report", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_QuantitativeAttributeAccuracy",
		      NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "measureDescription", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Doppler outside limits flag");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "result", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_ConformanceResult", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "specification", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_Citation", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ASF Seasat Data Quality document");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "date", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_Date", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
  node9 = xmlNewChild(node8, gmd, BAD_CAST "dateType", NULL);
  node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_DateTypeCode", 
		       BAD_CAST "publication");
  xmlNewProp(node10, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
  xmlNewProp(node10, BAD_CAST "codeListValue", BAD_CAST "publication");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "edition", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "version 1.0");

  node5 = xmlNewChild(node4, gmd, BAD_CAST "explanation", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Doppler outside tolerance level");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "pass", NULL);
  boolean2str(quality->dopplerOutsideLimitsFlag, str);
  xmlNewChild(node5, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - geolocation quality low flag
  node = xmlNewChild(section, gmd, BAD_CAST "report", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "DQ_QuantitativeAttributeAccuracy",
		      NULL);
  node3 = xmlNewChild(node2, gmd, BAD_CAST "measureDescription", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "geolocation quality low flag");
  node3 = xmlNewChild(node2, gmd, BAD_CAST "result", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "DQ_ConformanceResult", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "specification", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_Citation", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "title", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString",
	      BAD_CAST "ASF Seasat Data Quality document");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "date", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_Date", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
  node9 = xmlNewChild(node8, gmd, BAD_CAST "dateType", NULL);
  node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_DateTypeCode", 
		       BAD_CAST "publication");
  xmlNewProp(node10, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
  xmlNewProp(node10, BAD_CAST "codeListValue", BAD_CAST "publication");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "edition", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "version 1.0");

  node5 = xmlNewChild(node4, gmd, BAD_CAST "explanation", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "orbit/attitude/DEM/Doppler quality problems");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "pass", NULL);
  boolean2str(quality->geolocationQualityLowFlag, str);
  xmlNewChild(node5, gco, BAD_CAST "Boolean", BAD_CAST str);

  node = xmlNewChild(section, gmd, BAD_CAST "lineage", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "LI_Lineage", NULL);

  // dataQualityInfo - level 0 processing
  node3 = xmlNewChild(node2, gmd, BAD_CAST "processStep", NULL);
  node4 = xmlNewChild(node3, gmi, BAD_CAST "LE_ProcessStep", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "description", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString",	
	      BAD_CAST "Level0Processing");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "processor", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_ResponsibleParty", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "organisationName", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", 
	      BAD_CAST info->level0ProcessingFacility);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "role", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_RoleCode", BAD_CAST "processor");
  xmlNewProp(node8, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_RoleCode");
  xmlNewProp(node8, BAD_CAST "codeListValue", BAD_CAST "processor");
  node5 = xmlNewChild(node4, gmi, BAD_CAST "processingInformation", NULL);
  node6 = xmlNewChild(node5, eos, BAD_CAST "EOS_Processing", NULL);
  node7 = xmlNewChild(node6, gmi, BAD_CAST "identifier", NULL);

  node7 = xmlNewChild(node6, eos, BAD_CAST "otherPropertyType", NULL);
  node8 = xmlNewChild(node7, gco, BAD_CAST "RecordType", 
		       BAD_CAST "EOS Additional Attributes");
  xmlNewProp(node8, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
  node7 = xmlNewChild(node6, eos, BAD_CAST "otherProperty", NULL);
  node8 = xmlNewChild(node7, gco, BAD_CAST "Record", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributes", NULL);

  // dataQualityInfo - level 0 processing (chirpReplicaUsedFlag)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "chirpReplicaUsedFlag");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "BOOLEAN");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "reconstructed chirp used");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  boolean2str(proc->chirpReplicaUsedFlag, str);
  xmlNewChild(node14, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - level 0 processing (geometricDopplerUsedFlag)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "geometricDopplerUsedFlag");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "imageInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "BOOLEAN");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "geometric Doppler centroid estimate has been used");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  boolean2str(proc->geometricDopplerUsedFlag, str);
  xmlNewChild(node14, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - level 0 processing (dopplerBasebandEstimationMethod)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "dopplerBasebandEstimationMethod");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "imageInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "Doppler baseband estimation method");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  boolean2str(proc->geometricDopplerUsedFlag, str);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST proc->dopplerBasebandEstimationMethod);

  // dataQualityInfo - level 0 processing (azimuthPatternCorrectedFlag)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "azimuthPatternCorrectedFlag");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "imageInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "BOOLEAN");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "stripmap azimuth antenna pattern correction applied");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  boolean2str(proc->azimuthPatternCorrectedFlag, str);
  xmlNewChild(node14, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - level 0 processing (elevationPatternCorrectedFlag)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "elevationPatternCorrectedFlag");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "imageInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", BAD_CAST "imageInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "BOOLEAN");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "antenna elevation pattern correction applied");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  boolean2str(proc->elevationPatternCorrectedFlag, str);
  xmlNewChild(node14, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - level 0 processing steps
  for (ii=0; ii<setup->numProcessingSteps; ii++) {
    if (setup->processingStep[ii].processingLevel == PRE_PROCESSING ||
	setup->processingStep[ii].processingLevel == LEVEL_ZERO) {
      node5 = xmlNewChild(node4, gmd, BAD_CAST "processingInformation", NULL);
      node6 = xmlNewChild(node5, eos, BAD_CAST "EOS_Processing", NULL);
      node7 = xmlNewChild(node6, gmi, BAD_CAST "identifier", NULL);
      node8 = xmlNewChild(node7, gmd, BAD_CAST "MD_Identifier", NULL);
      node9 = xmlNewChild(node8, gmd, BAD_CAST "code", NULL);
      xmlNewChild(node9, gco, BAD_CAST "CharacterString", 
		  BAD_CAST setup->processingStep[ii].softwareID);
      node7 = xmlNewChild(node6, gmi, BAD_CAST "procedureDescription", NULL);
      xmlNewChild(node7, gco, BAD_CAST "CharacterString", 
		  BAD_CAST setup->processingStep[ii].description);
      node7 = xmlNewChild(node6, gmi, BAD_CAST "algorithm", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "EOS_Algorithm", NULL);
      node9 = xmlNewChild(node8, gmi, BAD_CAST "citation", NULL);
      node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_Citation", NULL);
      node11 = xmlNewChild(node10, gmd, BAD_CAST "title", NULL);
      xmlNewChild(node11, gco, BAD_CAST "CharacterString",
		  BAD_CAST setup->processingStep[ii].algorithm);
      node11 = xmlNewChild(node10, gmd, BAD_CAST "date", NULL);
      node12 = xmlNewChild(node11, gmd, BAD_CAST "CI_Date", NULL);
      node13 = xmlNewChild(node12, gmd, BAD_CAST "date", NULL);
      xmlNewChild(node13, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
      node13 = xmlNewChild(node12, gmd, BAD_CAST "dateType", NULL);
      node14 = xmlNewChild(node13, gmd, BAD_CAST "CI_DateTypeCode", 
			   BAD_CAST "publication");
      xmlNewProp(node14, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
      xmlNewProp(node14, BAD_CAST "codeListValue", BAD_CAST "publication");
      node11 = xmlNewChild(node10, gmd, BAD_CAST "edition", NULL);
      xmlNewChild(node11, gco, BAD_CAST "CharacterString",
		  BAD_CAST setup->processingStep[ii].softwareVersion);
      node9 = xmlNewChild(node8, gmi, BAD_CAST "description", NULL);
    }
  }

  // dataQualityInfo - level 1 processing
  node3 = xmlNewChild(node2, gmd, BAD_CAST "processStep", NULL);
  node4 = xmlNewChild(node3, gmi, BAD_CAST "LE_ProcessStep", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "description", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString",	
	      BAD_CAST "Level1Processing");
  node5 = xmlNewChild(node4, gmd, BAD_CAST "processor", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "CI_ResponsibleParty", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "organisationName", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", 
	      BAD_CAST info->level1ProcessingFacility);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "role", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "CI_RoleCode", BAD_CAST "processor");
  xmlNewProp(node8, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_RoleCode");
  xmlNewProp(node8, BAD_CAST "codeListValue", BAD_CAST "processor");
  node5 = xmlNewChild(node4, gmi, BAD_CAST "processingInformation", NULL);
  node6 = xmlNewChild(node5, eos, BAD_CAST "EOS_Processing", NULL);
  node7 = xmlNewChild(node6, gmi, BAD_CAST "identifier", NULL);

  node7 = xmlNewChild(node6, eos, BAD_CAST "otherPropertyType", NULL);
  node8 = xmlNewChild(node7, gco, BAD_CAST "RecordType", 
		       BAD_CAST "EOS Additional Attributes");
  xmlNewProp(node8, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
  node7 = xmlNewChild(node6, eos, BAD_CAST "otherProperty", NULL);
  node8 = xmlNewChild(node7, gco, BAD_CAST "Record", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributes", NULL);

  // dataQualityInfo - level 1 processing (imageCoordinateType)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "imageCoordinateType");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "RAW or ZERODOPPLER");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  if (spec->imageCoordinateType == RAW_COORD)
    strcpy(str, "RAW");
  else if (spec->imageCoordinateType == ZERODOPPLER)
    strcpy(str, "ZERODOPPLER");
  else if (spec->imageCoordinateType == UNDEF_COORD)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // dataQualityInfo - level 1 processing (radiometricCorrection)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "radiometricCorrection");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "CALIBRATED, RELATIVE CALIBRATED or NOT CALIBRATED");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  if (info->radiometricCorrection == CALIBRATED)
    strcpy(str, "CALIBRATED");
  else if (info->radiometricCorrection == RELCALIBRATED)
    strcpy(str, "RELATIVE CALIBRATED");
  else if (info->radiometricCorrection == NOTCALIBRATED)
    strcpy(str, "NOT CALIBRATED");
  else if (info->radiometricCorrection == UNDEF_CAL)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // dataQualityInfo - level 1 processing (projection)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST "projection");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "GROUNDRANGE, SLANTRANGE or MAP");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  if (info->projection == SLANTRANGE_PROJ)
    strcpy(str, "SLANTRANGE");
  else if (info->projection == GROUNDRANGE_PROJ)
    strcpy(str, "GROUNDRANGE");
  else if (info->projection == MAP_PROJ)
    strcpy(str, "MAP");
  else if (info->projection == UNDEF_PROJ)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // dataQualityInfo - level 1 processing (imageDataType)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "imageDataType");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "DETECTED, COMPLEX or RAW");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  if (info->imageDataType == DETECTED_DATA_TYPE)
    strcpy(str, "DETECTED");
  else if (info->imageDataType == COMPLEX_DATA_TYPE)
    strcpy(str, "COMPLEX");
  else if (info->imageDataType == RAW_DATA_TYPE)
    strcpy(str, "RAW");
  else if (info->imageDataType == UNDEF_DATA_TYPE)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // dataQualityInfo - level 1 processing (productVariant)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "productVariant");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "SLC, STD, TC, RTC, GEO, SSC, MGD, GEC or EEC");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
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
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // dataQualityInfo - level 1 processing (detectedFlag)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST "detectedFlag");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "BOOLEAN");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "detection performed");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  boolean2str(proc->detectedFlag, str);
  xmlNewChild(node14, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - level 1 processing (azimuthLooks)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST "azimuthLooks");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "number of azimuth looks");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  double2str(info->azimuthLooks, 0, str);
  xmlNewChild(node14, gco, BAD_CAST "Real", BAD_CAST str);

  // dataQualityInfo - level 1 processing (rangeLooks)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST "rangeLooks");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "number of range looks");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  double2str(info->rangeLooks, 0, str);
  xmlNewChild(node14, gco, BAD_CAST "Real", BAD_CAST str);

  // dataQualityInfo - level 1 processing (multiLookedFlag)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "multiLookedFlag");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "BOOLEAN");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "multilooking performed");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  boolean2str(proc->multiLookedFlag, str);
  xmlNewChild(node14, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - level 1 processing (nominalProcessingPerformedFlag)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "nominalProcessingPerformedFlag");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "processingInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "processingInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "BOOLEAN");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "nominal processing steps used");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  boolean2str(proc->nominalProcessingPerformedFlag, str);
  xmlNewChild(node14, gco, BAD_CAST "Boolean", BAD_CAST str);

  // dataQualityInfo - level 1 processing steps
  for (ii=0; ii<setup->numProcessingSteps; ii++) {
    if (setup->processingStep[ii].processingLevel == LEVEL_ONE) {
      node5 = xmlNewChild(node4, gmd, BAD_CAST "processingInformation", NULL);
      node6 = xmlNewChild(node5, eos, BAD_CAST "EOS_Processing", NULL);
      node7 = xmlNewChild(node6, gmi, BAD_CAST "identifier", NULL);
      node8 = xmlNewChild(node7, gmd, BAD_CAST "MD_Identifier", NULL);
      node9 = xmlNewChild(node8, gmd, BAD_CAST "code", NULL);
      xmlNewChild(node9, gco, BAD_CAST "CharacterString", 
		  BAD_CAST setup->processingStep[ii].softwareID);
      node7 = xmlNewChild(node6, gmi, BAD_CAST "procedureDescription", NULL);
      xmlNewChild(node7, gco, BAD_CAST "CharacterString", 
		  BAD_CAST setup->processingStep[ii].description);
      node7 = xmlNewChild(node6, gmi, BAD_CAST "algorithm", NULL);
      node8 = xmlNewChild(node7, eos, BAD_CAST "EOS_Algorithm", NULL);
      node9 = xmlNewChild(node8, gmi, BAD_CAST "citation", NULL);
      node10 = xmlNewChild(node9, gmd, BAD_CAST "CI_Citation", NULL);
      node11 = xmlNewChild(node10, gmd, BAD_CAST "title", NULL);
      xmlNewChild(node13, gco, BAD_CAST "CharacterString",
		  BAD_CAST setup->processingStep[ii].algorithm);
      node11 = xmlNewChild(node10, gmd, BAD_CAST "date", NULL);
      node12 = xmlNewChild(node11, gmd, BAD_CAST "CI_Date", NULL);
      node13 = xmlNewChild(node12, gmd, BAD_CAST "date", NULL);
      xmlNewChild(node14, gco, BAD_CAST "Date", BAD_CAST "2013-07-01");
      node13 = xmlNewChild(node12, gmd, BAD_CAST "dateType", NULL);
      node14 = xmlNewChild(node13, gmd, BAD_CAST "CI_DateTypeCode", 
			   BAD_CAST "publication");
      xmlNewProp(node14, BAD_CAST "codeList",
		 BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_DateTypeCode");
      xmlNewProp(node14, BAD_CAST "codeListValue", BAD_CAST "publication");
      node11 = xmlNewChild(node8, gmd, BAD_CAST "edition", NULL);
      xmlNewChild(node11, gco, BAD_CAST "CharacterString",
		  BAD_CAST setup->processingStep[ii].softwareVersion);
      node9 = xmlNewChild(node8, gmi, BAD_CAST "description", NULL);
    }
  }

  // dataQualityInfo - source
  node3 = xmlNewChild(node2, gmd, BAD_CAST "source", NULL);
  node4 = xmlNewChild(node3, gmi, BAD_CAST "LE_Source", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "description", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST header->source);
  
  node5 = xmlNewChild(node4, gmd, BAD_CAST "sourceExtent", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "EX_Extent", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "description", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "geographic and temporal extent of data frame");
  node7 = xmlNewChild(node6, gmd, BAD_CAST "EX_GeographicBoundingBox", NULL);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "extentTypeCode", NULL);
  xmlNewChild(node8, gco, BAD_CAST "Boolean", BAD_CAST "true");
  double westLon, eastLon, southLat, northLat;
  cornerCoords2boundingBox(info->sceneCornerCoord, 
			   &westLon, &eastLon, &southLat, &northLat);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "westBoundLongitude", NULL);
  sprintf(str, "%.5lf", westLon);
  xmlNewChild(node8, gco, BAD_CAST "Decimal", BAD_CAST str);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "eastBoundLongitude", NULL);
  sprintf(str, "%.5lf", eastLon);
  xmlNewChild(node8, gco, BAD_CAST "Decimal", BAD_CAST str);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "southBoundLatitude", NULL);
  sprintf(str, "%.5lf", southLat);
  xmlNewChild(node8, gco, BAD_CAST "Decimal", BAD_CAST str);
  node8 = xmlNewChild(node7, gmd, BAD_CAST "northBoundLatitude", NULL);
  sprintf(str, "%.5lf", northLat);
  xmlNewChild(node8, gco, BAD_CAST "Decimal", BAD_CAST str);

  node7 = xmlNewChild(node6, gmd, BAD_CAST "EX_TemporalExtent", NULL);
  xmlNewProp(node7, BAD_CAST "id", BAD_CAST "frameTemporalExtent");
  node8 = xmlNewChild(node7 , gmd, BAD_CAST "extent", NULL);
  node9 = xmlNewChild(node8, gmd, BAD_CAST "TimePeriod", NULL);
  dateTime2str(info->startTimeUTC, str);
  xmlNewChild(node9,gml, BAD_CAST "beginPosition", BAD_CAST str);
  dateTime2str(info->stopTimeUTC, str);
  xmlNewChild(node9, gml, BAD_CAST "endPosition", BAD_CAST str);

  node7 = xmlNewChild(node6, eos, BAD_CAST "otherPropertyType", NULL);
  node8 = xmlNewChild(node7, gco, BAD_CAST "RecordType", 
		      BAD_CAST "EOS Additional Attributes");
  xmlNewProp(node8, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
  node7 = xmlNewChild(node6, eos, BAD_CAST "otherProperty", NULL);
  node8 = xmlNewChild(node7, gco, BAD_CAST "Record", NULL);
  node9 = xmlNewChild(node8, eos, BAD_CAST "EOS_AdditionalAttributes", NULL);
  
  // dataQualityInfo - source (sceneAzimuthExtent)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "sceneAzimuthExtent");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "dataInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "dataInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "(approximate and at mid-range) scene extent in azimuth [m]");
  node14 = xmlNewChild(node13, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST "m");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  double2str(info->sceneAzimuthExtent, 3, str);
  xmlNewChild(node14, gco, BAD_CAST "Real", BAD_CAST str);
    
  // dataQualityInfo - source (sceneRangeExtent)
  node10 = xmlNewChild(node9, eos, BAD_CAST "additionalAttribute", NULL);
  node11 = xmlNewChild(node10, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node12 = xmlNewChild(node11, eos, BAD_CAST "reference", NULL);
  node13 = xmlNewChild(node12, eos, 
		       BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node14 = xmlNewChild(node13, eos, BAD_CAST "name", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "sceneRangeExtent");
  node14 = xmlNewChild(node13, eos, BAD_CAST "type", NULL);
  node15 = xmlNewChild(node14, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "dataInformation");
  xmlNewProp(node15, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node15, BAD_CAST "codeListValue", 
	     BAD_CAST "dataInformation");
  xmlNewChild(node13, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node14 = xmlNewChild(node13, eos, BAD_CAST "description", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "(approximate and at mid-range) scene extent in range [m]");
  node14 = xmlNewChild(node13, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node14, gco, BAD_CAST "CharacterString", BAD_CAST "m");
  node14 = xmlNewChild(node13, eos, BAD_CAST "value", NULL);
  double2str(info->sceneRangeExtent, 3, str);
  xmlNewChild(node14, gco, BAD_CAST "Real", BAD_CAST str);

  node5 = xmlNewChild(node4, gmi, BAD_CAST "processedLevel", NULL);
  node6 = xmlNewChild(node5, gmd, BAD_CAST "MD_Identifier", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "code", NULL);
  xmlNewChild(node7, gco, BAD_CAST "CharacterString", BAD_CAST "level one");
  
  node5 = xmlNewChild(node4, gmd, BAD_CAST "resolution", NULL);
  node6 = xmlNewChild(node5, gmi, BAD_CAST "LE_NominalResolution", NULL);
  node7 = xmlNewChild(node6, gmd, BAD_CAST "groundResolution", NULL);
  sprintf(str, "%.3lf", info->groundRangeResolution);
  node8 = xmlNewChild(node7, gco, BAD_CAST "Distance", BAD_CAST str);
  xmlNewProp(node8, BAD_CAST "uom", BAD_CAST "meter");
  xmlNewProp(node8, BAD_CAST "dimension", BAD_CAST "range");
  sprintf(str, "%.3lf", info->azimuthResolution);
  node8 = xmlNewChild(node7, gco, BAD_CAST "Distance", BAD_CAST str);
  xmlNewProp(node8, BAD_CAST "uom", BAD_CAST "meter");
  xmlNewProp(node8, BAD_CAST "dimension", BAD_CAST "azimuth");

  // metadataConstraints
  parent = xmlNewChild(mi_metadata, gmd, BAD_CAST "metadataConstraints", NULL);
  section = xmlNewChild(parent, gmd, BAD_CAST "MD_LegalConstraints", NULL);
  node = xmlNewChild(section, gmd, BAD_CAST "accessConstraints", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_RestrictionCode",
		      BAD_CAST "copyright");
  xmlNewProp(node2, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_RestrictionCode");
  xmlNewProp(node2, BAD_CAST "codeListValue", BAD_CAST "copyright");
  node = xmlNewChild(section, gmd, BAD_CAST "useConstraints", NULL);
  node2 = xmlNewChild(node, gmd, BAD_CAST "MD_RestrictionCode",
		      BAD_CAST "copyright");
  xmlNewProp(node2, BAD_CAST "codeList",
	     BAD_CAST "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_RestrictionCode");
  xmlNewProp(node2, BAD_CAST "codeListValue", BAD_CAST "copyright");

  // acquisitionInformation
  parent = xmlNewChild(mi_metadata, gmi, BAD_CAST "acquisitionInformation", 
		       NULL);  
  section = xmlNewChild(parent, eos, BAD_CAST "EOS_AcquisitionInformation", 
			NULL);

  // acquisitionInformation - instrument
  node = xmlNewChild(section, gmi, BAD_CAST "instrument", NULL);
  node2 = xmlNewChild(node, eos, BAD_CAST "EOS_Instrument", NULL);
  xmlNewProp(node2, BAD_CAST "id", BAD_CAST "sarInstrument");
  node3 = xmlNewChild(node2, gmi, BAD_CAST "identifier", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "MD_Identifier", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "code", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST info->sensor);
  node3 = xmlNewChild(node2, gmi, BAD_CAST "type", NULL);
  xmlNewChild(node3, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "C-band Synthetic Aperture Radar");
  node3 = xmlNewChild(node2, eos, BAD_CAST "otherPropertyType", NULL);
  node4 = xmlNewChild(node3, gco, BAD_CAST "RecordType", 
		      BAD_CAST "EOS Additional Attributes");
  xmlNewProp(node4, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
  node3 = xmlNewChild(node2, eos, BAD_CAST "otherProperty", NULL);
  node4 = xmlNewChild(node3, gco, BAD_CAST "Record", NULL);
  node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttributes", NULL);

  // acquisitionInformation - instrument (imageMode)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "imagingMode");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "FINE BEAM, STANDARD BEAM, STRIPMAP, SCANSAR or SPOTLIGHT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
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
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // acquisitionInformation - instrument (lookDirection)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "lookDirection");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "LEFT or RIGHT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  if (info->lookDirection == RIGHT_LOOK)
    strcpy(str, "RIGHT");
  else if (info->lookDirection == LEFT_LOOK)
    strcpy(str, "LEFT");
  else if (info->lookDirection == UNDEF_LOOK)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // acquisitionInformation - instrument (polarizationMode)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "polarizationMode");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "SINGLE, DUAL or QUAD");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  if (info->polarizationMode == SINGLE_POL)
    strcpy(str, "SINGLE");
  else if (info->polarizationMode == DUAL_POL)
    strcpy(str, "DUAL");
  else if (info->polarizationMode == QUAD_POL)
    strcpy(str, "QUAD");
  else if (info->polarizationMode == UNDEF_POL_MODE)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // acquisitionInformation - instrument (elevationBeamConfiguration)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "elevationBeamConfiguration");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "beam identification as taken from the order file");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST info->elevationBeamConfiguration);

  // acquisitionInformation - instrument (azimuthBeamID)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "azimuthBeamID");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "STRING");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "azimuth beam ID");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST info->azimuthBeamID);

  // acquisitionInformation - instrument (yawAngle)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "yawAngle");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "yaw angle of the instrument [degrees]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "degrees");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(info->yaw, 5, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (pitchAngle)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "pitchAngle");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "pitch angle of the instrument [degrees]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "degrees");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(info->pitch, 5, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (rollAngle)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "rollAngle");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "roll angle of the instrument [degrees]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "degrees");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(info->roll, 5, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (headingAngle)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "headingAngle");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "rotation of azimuth heading clockwise with respect to north [degrees]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "degrees");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(info->headingAngle, 5, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (earthRadius)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "earthRadius");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "earth radius at the scene center [m]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "m");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(info->earthRadius, 3, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (satelliteHeight)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "satelliteHeight");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "satellite height at the scene center from earth center [m]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "m");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(info->satelliteHeight, 3, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (chirpRate)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "chirpRate");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "chirp rate [Hz/s]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "Hz/s");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(proc->processingParameter[0].chirpRate, 0, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (pulseDuration)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "pulseDuration");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "pulse duration [Hz]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "Hz");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(proc->processingParameter[0].pulseDuration, 0, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (commonPRF)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "commonPRF");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "intermediate \"output PRF\" used during processing [Hz]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "Hz");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(spec->commonPRF, 0, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - instrument (commonRSF)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "commonRSF");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "range sampling frequency [Hz]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "Hz");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(spec->commonRSF, 0, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - platform
  node = xmlNewChild(section, gmi, BAD_CAST "platform", NULL);
  node2 = xmlNewChild(node, eos, BAD_CAST "EOS_Platform", NULL);
  node3 = xmlNewChild(node2, gmi, BAD_CAST "identifier", NULL);
  node4 = xmlNewChild(node3, gmd, BAD_CAST "MD_Identifier", NULL);
  node5 = xmlNewChild(node4, gmd, BAD_CAST "code", NULL);
  xmlNewChild(node5, gco, BAD_CAST "CharacterString", BAD_CAST header->mission);
  node3 = xmlNewChild(node2, gmi, BAD_CAST "description", NULL);
  node3 = xmlNewChild(node2, gmi, BAD_CAST "instrument", NULL);
  xmlNewProp(node3, BAD_CAST "xlink:href", BAD_CAST "#sarInstrument");
  node3 = xmlNewChild(node2, eos, BAD_CAST "otherPropertyType", NULL);
  node4 = xmlNewChild(node3, gco, BAD_CAST "RecordType", 
		      BAD_CAST "EOS Additional Attributes");
  xmlNewProp(node4, BAD_CAST "xlink:href", BAD_CAST "http://earthdata.nasa.gov/schemas/eos/eos.xsd#xpointer(//element[@name='EOS_AdditionalAttributes'])");
  node3 = xmlNewChild(node2, eos, BAD_CAST "otherProperty", NULL);
  node4 = xmlNewChild(node3, gco, BAD_CAST "Record", NULL);
  node5 = xmlNewChild(node4, eos, BAD_CAST "EOS_AdditionalAttributes", NULL);

  // acquisitionInformation - platform (orbitPhase)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "orbitPhase");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "orbit phase: 1 prelaunch phase, 0 launch phase, 1 nominal orbit");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  sprintf(str, "%d", info->orbitPhase);
  xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);
      
  // acquisitionInformation - platform (orbitCycle)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "orbitCycle");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "cycle number");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  sprintf(str, "%d", info->orbitCycle);
  xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);

  // acquisitionInformation - platform (absOrbit)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "absOrbit");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "absolute orbit number at the start of the scene");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  sprintf(str, "%d", info->absOrbit);
  xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);

  // acquisitionInformation - platform (relOrbit)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "relOrbit");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "relative orbit");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  sprintf(str, "%d", info->relOrbit);
  xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);

  // acquisitionInformation - platform (numOrbitsInCycle)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "numOrbitsInCycle");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "nominal number of orbits per cycle");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  sprintf(str, "%d", info->numOrbitsInCycle);
  xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);

  // acquisitionInformation - platform (orbitDirection)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "orbitDirection");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "ASCENDING or DESCENDING");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  if (info->orbitDirection == ASCENDING)
    strcpy(str, "ASCENDING");
  else if (info->orbitDirection == DESCENDING)
    strcpy(str, "DESCENDING");
  else if (info->orbitDirection == UNDEF_ORBIT)
    strcpy(str, "UNDEFINED");
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST str);

  // acquisitionInformation - platform (frameID)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "frameID");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "INT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "frame number");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  sprintf(str, "%d", setup->frameID);
  xmlNewChild(node10, gco, BAD_CAST "Integer", BAD_CAST str);

  // acquisitionInformation - platform (sceneCenterLatitude)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "sceneCenterLatitude");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "geographic latitude at scene center [degrees]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "degrees");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(info->sceneCenterCoord.lat, 5, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - platform (sceneCenterLongitude)
  node6 = xmlNewChild(node5, eos, BAD_CAST "additionalAttribute", NULL);
  node7 = xmlNewChild(node6, eos, BAD_CAST "EOS_AdditionalAttribute", NULL);
  node8 = xmlNewChild(node7, eos, BAD_CAST "reference", NULL);
  node9 = xmlNewChild(node8, eos, 
		      BAD_CAST "EOS_AdditionalAttributeDescription", NULL);
  node10 = xmlNewChild(node9, eos, BAD_CAST "name", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "sceneCenterLongitude");
  node10 = xmlNewChild(node9, eos, BAD_CAST "type", NULL);
  node11 = xmlNewChild(node10, eos, 
		       BAD_CAST "EOS_AdditionalAttributeTypeCode",
		       BAD_CAST "acquisitionInformation");
  xmlNewProp(node11, BAD_CAST "codeList", 
	     BAD_CAST "http://earthdata.nasa.gov/metadata/resources/Codelist.xml#EOS_AdditionalAttributeTypeCode");
  xmlNewProp(node11, BAD_CAST "codeListValue", 
	     BAD_CAST "acquisitionInformation");
  xmlNewChild(node9, eos, BAD_CAST "dataType", BAD_CAST "FLOAT");
  node10 = xmlNewChild(node9, eos, BAD_CAST "description", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", 
	      BAD_CAST "geographic longitude at scene center [degrees]");
  node10 = xmlNewChild(node9, eos, BAD_CAST "parameterUnitsOfMeasure", NULL);
  xmlNewChild(node10, gco, BAD_CAST "CharacterString", BAD_CAST "degrees");
  node10 = xmlNewChild(node9, eos, BAD_CAST "value", NULL);
  double2str(info->sceneCenterCoord.lon, 5, str);
  xmlNewChild(node10, gco, BAD_CAST "Real", BAD_CAST str);

  // acquisitionInformation - position
  node = xmlNewChild(section, gmi, BAD_CAST "position", NULL);

  // Save tree to file
  xmlSaveFormatFileEnc(outFile, doc, "UTF-8", 1);
  
  // Clean up
  xmlFreeDoc(doc);
  xmlCleanupParser();
}
