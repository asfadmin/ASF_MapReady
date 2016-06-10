#include "asf_meta.h"
#include "doppler.h"
#include "meta_init.h"
#include "dateUtil.h"
#include "xml_util.h"
#include "asf_nan.h"
#include "sentinel.h"
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

sentinel_meta *sentinel_meta_init(void)
{
  sentinel_meta *sentinel;
  sentinel = (sentinel_meta *) CALLOC(1, sizeof(sentinel_meta));

  strcpy(sentinel->granule, MAGIC_UNSET_STRING);
  strcpy(sentinel->familyName, MAGIC_UNSET_STRING);
  strcpy(sentinel->number, MAGIC_UNSET_STRING);
  strcpy(sentinel->instrument, MAGIC_UNSET_STRING);
  strcpy(sentinel->mode, MAGIC_UNSET_STRING);
  strcpy(sentinel->processor, MAGIC_UNSET_STRING);
  strcpy(sentinel->version, MAGIC_UNSET_STRING);
  sentinel->orbitNumber = MAGIC_UNSET_INT;
  sentinel->relativeOrbitNumber = MAGIC_UNSET_INT;
  sentinel->cycleNumber = MAGIC_UNSET_INT;
  strcpy(sentinel->pass, MAGIC_UNSET_STRING);
  strcpy(sentinel->polarization, MAGIC_UNSET_STRING);
  strcpy(sentinel->productType, MAGIC_UNSET_STRING);
  strcpy(sentinel->startTime, MAGIC_UNSET_STRING);
  strcpy(sentinel->stopTime, MAGIC_UNSET_STRING);
  strcpy(sentinel->mission, MAGIC_UNSET_STRING);
  strcpy(sentinel->projection, MAGIC_UNSET_STRING);
  sentinel->rangeSamplingRate = MAGIC_UNSET_DOUBLE;
  sentinel->radarFrequency = MAGIC_UNSET_DOUBLE;
  sentinel->roll = MAGIC_UNSET_DOUBLE;
  sentinel->pitch = MAGIC_UNSET_DOUBLE;
  sentinel->yaw = MAGIC_UNSET_DOUBLE;
  sentinel->slantRangeTime = MAGIC_UNSET_DOUBLE;
  strcpy(sentinel->pixelValue, MAGIC_UNSET_STRING);
  strcpy(sentinel->outputPixels, MAGIC_UNSET_STRING);
  sentinel->rangePixelSpacing = MAGIC_UNSET_DOUBLE;
  sentinel->azimuthPixelSpacing = MAGIC_UNSET_DOUBLE;
  sentinel->azimuthTimeInterval = MAGIC_UNSET_DOUBLE;
  sentinel->numberOfSamples = MAGIC_UNSET_INT;
  sentinel->numberOfLines = MAGIC_UNSET_INT;
  sentinel->incidenceAngleMidSwath = MAGIC_UNSET_DOUBLE;
  strcpy(sentinel->ellipsoidName, MAGIC_UNSET_STRING);
  sentinel->ellipsoidSemiMajorAxis = MAGIC_UNSET_DOUBLE;
  sentinel->ellipsoidSemiMinorAxis = MAGIC_UNSET_DOUBLE;
  sentinel->vector_count = 0;
  sentinel->stVec = NULL;
  sentinel->gcp_count = 0;
  sentinel->gcp = NULL;
  sentinel->band_count = 0;
  sentinel->data = NULL;
  sentinel->polarization_count = 0;
  sentinel->file = NULL;

  return sentinel;
}

void remove_namespace(xmlNode *node)
{
  xmlNode *curNode = NULL;
  for (curNode = node; curNode; curNode = curNode->next) {
    curNode->ns = NULL;
    remove_namespace(curNode->children);
  }
}

void check_sentinel_meta(const char *fileName, char *mission, char *beamMode, 
  char *productType)
{
  char manifest[1024], familyName[15], number[3];

  if (!fileExists(fileName))
    asfPrintError("Metadata file (%s) does not exist!\n", fileName);
  realpath(fileName, manifest);
  xmlDoc *doc = xmlReadFile(manifest, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", manifest);
 
  xmlNode *root = xmlDocGetRootElement(doc);
  remove_namespace(root);
 
  strcpy(familyName, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/xmlData/"
    "platform/familyName"));
  strcpy(number, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/xmlData/"
    "platform/number"));
  sprintf(mission, "%s%s", familyName, number);
  strcpy(beamMode, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/xmlData/"
    "platform/instrument/extension/instrumentMode/mode"));
  if (strcmp_case(beamMode, "SM") == 0)
    strcpy(beamMode, xml_xpath_get_string_value(doc, 
      "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/"
      "xmlData/platform/instrument/extension/instrumentMode/swath"));  
  strcpy(productType, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='generalProductInformation']/"
    "metadataWrap/xmlData/standAloneProductInformation/productType"));
}

static void verify_gcps(gcp_location *gcp, int gcp_count)
{
  int ii, nPos=0, nNeg=0, left=0, right=0;
  double diff=0, leftDiff=0, rightDiff=0;
  
  for (ii=0; ii<gcp_count; ii++) {
    if (gcp[ii].lon > 0.0) {
      diff = 180.0 - gcp[ii].lon;
      if (diff > rightDiff) {
        rightDiff = diff;
        right = ii;
      }
      nPos++;
    }
    else {
      diff = gcp[ii].lon + 180.0;
      if (diff > leftDiff) {
        leftDiff = diff;
        left = ii;
      }
      nNeg++;
    }
  }
  diff = gcp[right].lon - gcp[left].lon;

  // only action required when data crosses the dateline
  if (nPos > 0 && nNeg > 0 && diff > 180.0) {
    for (ii=0; ii<gcp_count; ii++) {
      if (nPos > nNeg) {
        if (gcp[ii].lon < 0.0)
          gcp[ii].lon += 360.0;
      }
      else {
        if (gcp[ii].lon > 0.0)
          gcp[ii].lon -= 360.0;
      }
    }
  }
}

sentinel_meta *read_sentinel_meta(const char *fileName, int channel)
{
  int ii=0, kk, n;
  char str[512], id[128], annotation[1024], file[512];
  char absPath[1024], manifest[1024], href[1024], **arr;

  sentinel_meta *sentinel = sentinel_meta_init();

  if (!fileExists(fileName))
    asfPrintError("Metadata file (%s) does not exist!\n", fileName);
  realpath(fileName, manifest);
  split_dir_and_file(manifest, absPath, file);
  split_into_array(absPath, '/', &n, &arr);
  strncpy(sentinel->granule, arr[n-2], strlen(arr[n-2])-5);
  sentinel->resolution = sentinel->granule[10];

  // Read manifest
  //asfPrintStatus("\n   Reading manifest ...\n");
  xmlDoc *doc = xmlReadFile(manifest, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", manifest);
 
  xmlNode *root = xmlDocGetRootElement(doc);
  if (!root)
    asfPrintError("Could not get root element %s\n", manifest);
  remove_namespace(root);
 
  strcpy(sentinel->familyName, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/xmlData/"
    "platform/familyName"));
  strcpy(sentinel->number, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/xmlData/"
    "platform/number"));
  strcpy(sentinel->instrument, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/xmlData/"
    "platform/instrument/familyName/@abbreviation"));
  strcpy(sentinel->mode, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/xmlData/"
    "platform/instrument/extension/instrumentMode/mode"));
  if (strcmp_case(sentinel->mode, "SM") == 0)
    strcpy(sentinel->mode, xml_xpath_get_string_value(doc, 
      "/XFDU/metadataSection/metadataObject[@ID='platform']/metadataWrap/"
      "xmlData/platform/instrument/extension/instrumentMode/swath"));
  strcpy(sentinel->missionDataTakeID, xml_xpath_get_string_value(doc, 
    "/XFDU/metadataSection/metadataObject[@ID='generalProductInformation']/"
    "metadataWrap/xmlData/standAloneProductInformation/missionDataTakeID"));
  strcpy(sentinel->productClass, xml_xpath_get_string_value(doc, 
    "/XFDU/metadataSection/metadataObject[@ID='generalProductInformation']/"
    "metadataWrap/xmlData/standAloneProductInformation/productClass"));
  strcpy(sentinel->processor, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='processing']/metadataWrap/"
    "xmlData/processing/facility/software/@name"));
  strcpy(sentinel->version, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='processing']/metadataWrap/"
    "xmlData/processing/facility/software/@version"));
  sentinel->orbitNumber = xml_xpath_get_int_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='measurementOrbitReference']/"
    "metadataWrap/xmlData/orbitReference/orbitNumber[@type='start']");
  sentinel->relativeOrbitNumber = xml_xpath_get_int_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='measurementOrbitReference']/"
    "metadataWrap/xmlData/orbitReference/relativeOrbitNumber[@type='start']");
  sentinel->cycleNumber = xml_xpath_get_int_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='measurementOrbitReference']/"
    "metadataWrap/xmlData/orbitReference/cycleNumber");
  strcpy(sentinel->pass, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='measurementOrbitReference']/"
    "metadataWrap/xmlData/orbitReference/extension/orbitProperties/pass"));
  strcpy(sentinel->productType, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='generalProductInformation']/"
    "metadataWrap/xmlData/standAloneProductInformation/productType"));
  char pol1[5], pol2[5];
  strcpy(pol1, xml_xpath_get_string_value(doc, "/XFDU/metadataSection/"
    "metadataObject[@ID='generalProductInformation']/metadataWrap/xmlData/"
    "standAloneProductInformation/transmitterReceiverPolarisation[1]"));
  strcpy(pol2, MAGIC_UNSET_STRING);
  if (xml_xpath_element_exists(doc, "/XFDU/metadataSection/"
      "metadataObject[@ID='generalProductInformation']/metadataWrap/xmlData/"
      "standAloneProductInformation/transmitterReceiverPolarisation[2]"))
    strcpy(pol2, xml_xpath_get_string_value(doc, "/XFDU/metadataSection/"
      "metadataObject[@ID='generalProductInformation']/metadataWrap/xmlData/"
      "standAloneProductInformation/transmitterReceiverPolarisation[2]"));
  if (strcmp_case(sentinel->productType, "SLC") == 0) {
    if (strcmp_case(pol2, MAGIC_UNSET_STRING) != 0)
      sprintf(sentinel->polarization, "%s-AMP,%s-PHASE,%s-AMP,%s-PHASE", 
        pol1, pol1, pol2, pol2);
    else
      sprintf(sentinel->polarization, "%s-AMP,%s-PHASE", pol1, pol1);
  }
  else {
    if (strcmp_case(pol2, MAGIC_UNSET_STRING) != 0)
      sprintf(sentinel->polarization, "%s,%s", pol1, pol2);
    else
      strcpy(sentinel->polarization, pol1);
  }
  strcpy(sentinel->startTime, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='acquisitionPeriod']/"
    "metadataWrap/xmlData/acquisitionPeriod/startTime"));
  strcpy(sentinel->stopTime, xml_xpath_get_string_value(doc,
    "/XFDU/metadataSection/metadataObject[@ID='acquisitionPeriod']/"
    "metadataWrap/xmlData/acquisitionPeriod/stopTime"));

  char **files;
  sentinel->data = (char **) MALLOC(sizeof(char *)*255);
  for (ii=0; ii<255; ii++) {
    sentinel->data[ii] = (char *) MALLOC(sizeof(char)*1024);
    strcpy(sentinel->data[ii], MAGIC_UNSET_STRING);
  }
  int count = xml_xpath_get_count(doc, "/XFDU/dataObjectSection/dataObject");
  files = (char **) MALLOC(sizeof(char *)*count);
  for (ii=0; ii<count; ii++)
    files[ii] = (char *) MALLOC(sizeof(char)*1024);
  char beamMode[10];
  strcpy(beamMode, "");
  if (strcmp_case(sentinel->productType, "SLC") == 0)
    sprintf(beamMode, "%s%d", lc(sentinel->mode), channel);
  else
    sprintf(beamMode, "%s", lc(sentinel->mode));
    
  // Collect the names of all ancillary files
  int pol_count = 1;
  if (strcmp_case(pol2, MAGIC_UNSET_STRING) != 0)
    pol_count = 2;
  sentinel->polarization_count = pol_count;
  sentinel->file = (sentinel_files *) MALLOC(sizeof(sentinel_files)*pol_count);
  strcpy(sentinel->file[0].polarization, pol1);
  if (pol_count == 2)
    strcpy(sentinel->file[1].polarization, pol2);
  for (ii=0; ii<count; ii++) {
    sprintf(str, "/XFDU/dataObjectSection/dataObject[%d]/@ID", ii+1);
    strcpy(id, xml_xpath_get_string_value(doc, str));
    sprintf(str, "/XFDU/dataObjectSection/dataObject[%d]/byteStream/"
      "fileLocation/@href", ii+1);
    strcpy(href, xml_xpath_get_string_value(doc, str));
    if (strncmp_case(id, "product", 7) == 0 && strstr(href, "annotation")) {
      if (strstr(id, lc(pol1)))
        sprintf(sentinel->file[0].annotation, "%s%s", absPath, &href[2]);
      else if (strcmp_case(pol2, MAGIC_UNSET_STRING) != 0 && 
        strstr(id, lc(pol2)))
        sprintf(sentinel->file[1].annotation, "%s%s", absPath, &href[2]);
      sprintf(annotation, "%s%s", absPath, &href[2]);
    }
    if (strncmp_case(id, "noise", 5) == 0 && strstr(href, "annotation")) {
      if (strstr(id, lc(pol1)))
        sprintf(sentinel->file[0].noise, "%s%s", absPath, &href[2]);
      else if (strcmp_case(pol2, MAGIC_UNSET_STRING) != 0 &&
        strstr(id, lc(pol2)))
        sprintf(sentinel->file[1].noise, "%s%s", absPath, &href[2]);
    }
    if (strncmp_case(id, "calibration", 11) == 0 && 
      strstr(href, "annotation")) {
      if (strstr(id, lc(pol1)))
        sprintf(sentinel->file[0].calibration, "%s%s", absPath, &href[2]);
      else if (strcmp_case(pol2, MAGIC_UNSET_STRING) != 0 &&
        strstr(id, lc(pol2)))
        sprintf(sentinel->file[1].calibration, "%s%s", absPath, &href[2]);
    }
    strcpy(files[ii], href);
  }
  
  // Collect the names of actual data sets
  char tifStr[15], ncStr[15];
  int fileCount = 0;
  for (kk=0; kk<255; kk++) {
    sprintf(tifStr, "%03d.tiff", kk+1);
    sprintf(ncStr, "%03d.nc", kk+1);
    for (ii=0; ii<count; ii++) {
      strcpy(href, files[ii]);
      if (strstr(href, "measurement") && strstr(href, beamMode) &&
        (strstr(href, tifStr) || strstr(href, ncStr))) {
        sprintf(sentinel->data[fileCount], "%s%s", absPath, &href[2]);
        fileCount++;
      }
    }
  }
  if (strcmp_case(sentinel->productType, "OCN") == 0) {
    char location[512];
    int gcp_count = fileCount*4;
    sentinel->gcp = (gcp_location *) MALLOC(sizeof(gcp_location)*gcp_count);
    for (ii=0; ii<fileCount; ii++) {
      sprintf(str, "/XFDU/metadataSection/"
        "metadataObject[@ID='measurementFrameSet']/metadataWrap/xmlData/"
        "frameSet/frame[%d]/footPrint/coordinates", ii+1);
      strcpy(location, xml_xpath_get_string_value(doc, str));
      split_into_array(location, ' ', &n, &arr);
      //sentinel->gcp[ii].lat = xml_xpath_get_double_value(doc, str);
      //sentinel->gcp[ii].lon = xml_xpath_get_double_value(doc, str);
    }
    //verify_gcps(sentinel->gcp, gcp_count);
    sentinel->gcp_count = gcp_count;
  }
  
  for (ii=0; ii<count; ii++)
    FREE(files[ii]);
  FREE(files);
  sentinel->file_count = fileCount;
  xmlFreeDoc(doc);
  xmlCleanupParser();
  //printf("Found %d files\n", fileCount);
  
  if (strcmp_case(sentinel->productType, "SLC") == 0 ||
      strcmp_case(sentinel->productType, "GRD") == 0) {
    // Reading annotation file
    asfPrintStatus("\n   Reading annotation file (%s) ...\n", annotation);
    doc = xmlReadFile(annotation, NULL, 0);
    if (!doc)
      asfPrintError("Could not parse file %s\n", annotation);
   
    root = xmlDocGetRootElement(doc);
    remove_namespace(root);
  
    strcpy(sentinel->mission, xml_xpath_get_string_value(doc, 
      "/product/adsHeader/missionId"));
    strcpy(sentinel->projection, xml_xpath_get_string_value(doc, 
      "/product/generalAnnotation/productInformation/projection"));
    sentinel->rangeSamplingRate = xml_xpath_get_double_value(doc, 
      "/product/generalAnnotation/productInformation/rangeSamplingRate");
    sentinel->radarFrequency = xml_xpath_get_double_value(doc, 
      "/product/generalAnnotation/productInformation/radarFrequency");
    
    // Average roll, pitch and yaw values
    int attitude_count = xml_xpath_get_int_value(doc, 
      "/product/generalAnnotation/attitudeList/@count");
    double roll, pitch, yaw;
    for (ii=0; ii<attitude_count; ii++) {
      sprintf(str, "/product/generalAnnotation/attitudeList/attitude[%d]/roll", 
        ii+1);
      roll += xml_xpath_get_double_value(doc, str);
      sprintf(str, "/product/generalAnnotation/attitudeList/attitude[%d]/pitch", 
        ii+1);
      pitch += xml_xpath_get_double_value(doc, str);
      sprintf(str, "/product/generalAnnotation/attitudeList/attitude[%d]/yaw", 
        ii+1);
      yaw += xml_xpath_get_double_value(doc, str);
    }
    sentinel->roll = roll/attitude_count;
    sentinel->pitch = pitch/attitude_count;
    sentinel->yaw = yaw/attitude_count;
    sentinel->slantRangeTime = xml_xpath_get_double_value(doc, 
      "/product/imageAnnotation/imageInformation/slantRangeTime");
    strcpy(sentinel->pixelValue, xml_xpath_get_string_value(doc, 
      "/product/imageAnnotation/imageInformation/pixelValue"));
    strcpy(sentinel->outputPixels, xml_xpath_get_string_value(doc, 
      "/product/imageAnnotation/imageInformation/outputPixels"));
    sentinel->rangePixelSpacing = xml_xpath_get_double_value(doc, 
      "/product/imageAnnotation/imageInformation/rangePixelSpacing");
    sentinel->azimuthPixelSpacing = xml_xpath_get_double_value(doc, 
      "/product/imageAnnotation/imageInformation/azimuthPixelSpacing");
    sentinel->azimuthTimeInterval = xml_xpath_get_double_value(doc, 
      "/product/imageAnnotation/imageInformation/azimuthTimeInterval");
    sentinel->numberOfSamples = xml_xpath_get_int_value(doc, 
      "/product/imageAnnotation/imageInformation/numberOfSamples");
    sentinel->numberOfLines = xml_xpath_get_int_value(doc, 
      "/product/imageAnnotation/imageInformation/numberOfLines");
    sentinel->incidenceAngleMidSwath = xml_xpath_get_double_value(doc, 
      "/product/imageAnnotation/imageInformation/incidenceAngleMidSwath");
    strcpy(sentinel->ellipsoidName , xml_xpath_get_string_value(doc, 
      "/product/imageAnnotation/processingInformation/ellipsoidName"));
    sentinel->ellipsoidSemiMajorAxis = xml_xpath_get_double_value(doc, 
      "/product/imageAnnotation/processingInformation/ellipsoidSemiMajorAxis");
    sentinel->ellipsoidSemiMinorAxis = xml_xpath_get_double_value(doc, 
      "/product/imageAnnotation/processingInformation/ellipsoidSemiMinorAxis");
    
    // Read state vectors
    int vector_count = xml_xpath_get_int_value(doc, 
      "/product/generalAnnotation/orbitList/@count");
    sentinel->stVec = 
      (sentinel_vector *) MALLOC(sizeof(sentinel_vector)*vector_count);
    for (ii=0; ii<vector_count; ii++) {
      sprintf(str, "/product/generalAnnotation/orbitList/orbit[%d]/time", ii+1);
      strcpy(sentinel->stVec[ii].time, xml_xpath_get_string_value(doc, str));
      sprintf(str, "/product/generalAnnotation/orbitList/orbit[%d]/position/x", 
        ii+1);
      sentinel->stVec[ii].posX = xml_xpath_get_double_value(doc, str);
      sprintf(str, "/product/generalAnnotation/orbitList/orbit[%d]/position/y", 
        ii+1);
      sentinel->stVec[ii].posY = xml_xpath_get_double_value(doc, str);
      sprintf(str, "/product/generalAnnotation/orbitList/orbit[%d]/position/z", 
        ii+1);
      sentinel->stVec[ii].posZ = xml_xpath_get_double_value(doc, str);
      sprintf(str, "/product/generalAnnotation/orbitList/orbit[%d]/velocity/x", 
        ii+1);
      sentinel->stVec[ii].velX = xml_xpath_get_double_value(doc, str);
      sprintf(str, "/product/generalAnnotation/orbitList/orbit[%d]/velocity/y", 
        ii+1);
      sentinel->stVec[ii].velY = xml_xpath_get_double_value(doc, str);
      sprintf(str, "/product/generalAnnotation/orbitList/orbit[%d]/velocity/z", 
        ii+1);
      sentinel->stVec[ii].velZ = xml_xpath_get_double_value(doc, str);
    }
    sentinel->vector_count = vector_count;
    
    // Read ground control points
    int gcp_count = xml_xpath_get_int_value(doc, "/product/geolocationGrid/"
      "geolocationGridPointList/@count");
    sentinel->gcp = (gcp_location *) MALLOC(sizeof(gcp_location)*gcp_count);
    for (ii=0; ii<gcp_count; ii++) {
      sprintf(str, "/product/geolocationGrid/geolocationGridPointList/"
        "geolocationGridPoint[%d]/line", ii+1);
      sentinel->gcp[ii].line = xml_xpath_get_int_value(doc, str);
      sprintf(str, "/product/geolocationGrid/geolocationGridPointList/"
        "geolocationGridPoint[%d]/pixel", ii+1);
      sentinel->gcp[ii].pixel = xml_xpath_get_int_value(doc, str);
      sprintf(str, "/product/geolocationGrid/geolocationGridPointList/"
        "geolocationGridPoint[%d]/latitude", ii+1);
      sentinel->gcp[ii].lat = xml_xpath_get_double_value(doc, str);
      sprintf(str, "/product/geolocationGrid/geolocationGridPointList/"
        "geolocationGridPoint[%d]/longitude", ii+1);
      sentinel->gcp[ii].lon = xml_xpath_get_double_value(doc, str);
    }
    verify_gcps(sentinel->gcp, gcp_count);
    sentinel->gcp_count = gcp_count;

    // Update start/stop time with GCP times for first pixel in first and last
    // line
    for (ii=0; ii<gcp_count; ii++) {
      if (sentinel->gcp[ii].line == 0 && sentinel->gcp[ii].pixel == 0) {
        sprintf(str, "/product/geolocationGrid/geolocationGridPointList/"
          "geolocationGridPoint[%d]/azimuthTime", ii+1);
        strcpy(sentinel->startTime, xml_xpath_get_string_value(doc, str));
      }
      else if (sentinel->gcp[ii].line == sentinel->numberOfLines-1 && 
        sentinel->gcp[ii].pixel == 0) {
        sprintf(str, "/product/geolocationGrid/geolocationGridPointList/"
          "geolocationGridPoint[%d]/azimuthTime", ii+1);
        strcpy(sentinel->stopTime, xml_xpath_get_string_value(doc, str));
      }
    }
    
    xmlFreeDoc(doc);
    xmlCleanupParser();
  }
  else if (strcmp_case(sentinel->productType, "OCN") == 0) {
    if (strcmp_case(sentinel->familyName, "Sentinel-1") == 0 && 
      strcmp_case(sentinel->number, "A") == 0)
      strcpy(sentinel->mission, "S1A");
    else if (strcmp_case(sentinel->familyName, "Sentinel-1") == 0 && 
      strcmp_case(sentinel->number, "B") == 0)
      strcpy(sentinel->mission, "S1B");
  }
  
  return sentinel;
}

meta_parameters* sentinel2meta(sentinel_meta *sentinel)
{
  int ii;
  double lat, lon, re, rp;
  ymd_date imgStartDate, imgStopDate, vecYmd;
  hms_time imgStartTime, imgStopTime, vecHms;

  // Allocate memory for metadata structure
  meta_parameters *meta = raw_init();
  
  // General block
  strcpy(meta->general->basename, sentinel->granule);
  sprintf(meta->general->sensor, "%s%s", 
    sentinel->familyName, sentinel->number);
  strcpy(meta->general->sensor_name, sentinel->instrument);
  strcpy(meta->general->mode, sentinel->mode);
  sprintf(meta->general->processor, "%s %s", 
    sentinel->processor, sentinel->version);
  meta->general->data_type = REAL32;
  if (strcmp_case(sentinel->productType, "RAW") == 0)
    meta->general->image_data_type = RAW_IMAGE;
  else if (strcmp_case(sentinel->productType, "SLC") == 0) {
    meta->general->image_data_type = COMPLEX_IMAGE;
    meta->sar = meta_sar_init();
    meta->state_vectors = meta_state_vectors_init(sentinel->vector_count);
  }
  else if (strcmp_case(sentinel->productType, "GRD") == 0) {
    meta->general->image_data_type = AMPLITUDE_IMAGE;
    meta->sar = meta_sar_init();
    meta->state_vectors = meta_state_vectors_init(sentinel->vector_count);
  }
  else if (strcmp_case(sentinel->productType, "OCN") == 0)
    meta->general->image_data_type = MODEL_OUTPUT;
  meta->general->radiometry = r_AMP;
  sprintf(meta->general->acquisition_date, "%sZ", sentinel->startTime);
  meta->general->orbit = sentinel->orbitNumber;
  if (strcmp_case(sentinel->pass, "ASCENDING") == 0)
    meta->general->orbit_direction = 'A';
  else if (strcmp_case(sentinel->pass, "DESCENDING") == 0)
    meta->general->orbit_direction = 'D';
  strcpy(meta->general->bands, sentinel->polarization);
  meta->general->line_count = sentinel->numberOfLines;
  meta->general->sample_count = sentinel->numberOfSamples;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = sentinel->rangePixelSpacing;
  meta->general->y_pixel_size = sentinel->azimuthPixelSpacing;
  meta->general->re_major = sentinel->ellipsoidSemiMajorAxis;
  meta->general->re_minor = sentinel->ellipsoidSemiMinorAxis;

  // SAR block
  if (meta->sar) {
    if (strcmp_case(sentinel->productType, "SLC") == 0)
      meta->sar->image_type = 'S';
    else if (strcmp_case(sentinel->productType, "GRD") == 0)
      meta->sar->image_type = 'G';
    snprintf(meta->sar->polarization, 3, "%s", &(sentinel->granule)[14]);
    if (strcmp_case(meta->sar->polarization, "DH") == 0 ||
        strcmp_case(meta->sar->polarization, "DV") == 0)
      meta->general->band_count = 2;
    else
      meta->general->band_count = 1;
    if (strcmp_case(sentinel->productType, "SLC") == 0)
      meta->general->band_count *= 2;
    meta->sar->look_direction = 'R';
    if (strcmp_case(meta->general->mode, "IW") == 0) {
      meta->sar->azimuth_look_count = 5;
      meta->sar->range_look_count = 1;
    }
    else if (strcmp_case(meta->general->mode, "EW") == 0) {
      meta->sar->azimuth_look_count = 6;
      meta->sar->range_look_count = 2;
    }
    else {
      meta->sar->azimuth_look_count = 1;
      meta->sar->range_look_count = 1;
    }
    meta->sar->deskewed = 1;
    meta->sar->original_line_count = meta->general->line_count;
    meta->sar->original_sample_count = meta->general->sample_count;
    meta->sar->line_increment = 1;
    meta->sar->sample_increment = 1;
    meta->sar->azimuth_time_per_pixel = sentinel->azimuthTimeInterval;
    meta->sar->range_time_per_pixel = 
      meta->general->x_pixel_size * 2.0 / speedOfLight;
    meta->sar->slant_range_first_pixel = 
      sentinel->slantRangeTime * speedOfLight / 2.0;
    meta->sar->slant_shift = 0.0;
    meta->sar->time_shift = 0.0;
    meta->sar->wavelength = speedOfLight / sentinel->radarFrequency;
    // PRF varies for each swath
    meta->sar->prf = MAGIC_UNSET_DOUBLE;
    //meta->sar->satellite_height = sentinel->satelliteHeight;
    if (strcmp_case(sentinel->productType, "GRD") == 0)
      meta->sar->multilook = TRUE;
    else if (strcmp_case(sentinel->productType, "SLC") == 0) {
      if (strcmp(sentinel->mode, "IW") == 0 || 
        strcmp_case(sentinel->mode, "EW") == 0)
        meta->sar->multilook = FALSE;      
      else
        meta->sar->multilook = TRUE;
    }
    meta->sar->pitch = sentinel->pitch;
    meta->sar->roll = sentinel->roll;
    meta->sar->yaw = sentinel->yaw;
    meta->sar->range_sampling_rate = sentinel->rangeSamplingRate;
    meta->sar->incid_a[0] = sentinel->incidenceAngleMidSwath;
  }

  // Doppler block
  //meta->doppler = radarsat2->doppler;

  // State vectors
  if (meta->state_vectors) {
    julian_date jd;
    iso2date(sentinel->startTime, &imgStartDate, &imgStartTime);
    iso2date(sentinel->stopTime, &imgStopDate, &imgStopTime);
    date_ymd2jd(&imgStartDate,  &jd);
    meta->state_vectors->vector_count = sentinel->vector_count;
    meta->state_vectors->year = imgStartDate.year;
    meta->state_vectors->julDay = jd.jd;
    meta->state_vectors->second = date_hms2sec(&imgStartTime);
    meta->state_vectors->num = meta->state_vectors->vector_count;
    for (ii=0; ii<meta->state_vectors->vector_count; ii++) {
      iso2date(sentinel->stVec[ii].time, &vecYmd, &vecHms);
      meta->state_vectors->vecs[ii].time = 
        time_difference(&vecYmd, &vecHms, &imgStartDate, &imgStartTime);
      meta->state_vectors->vecs[ii].vec.pos.x = sentinel->stVec[ii].posX;
      meta->state_vectors->vecs[ii].vec.pos.y = sentinel->stVec[ii].posY;
      meta->state_vectors->vecs[ii].vec.pos.z = sentinel->stVec[ii].posZ;
      meta->state_vectors->vecs[ii].vec.vel.x = sentinel->stVec[ii].velX;
      meta->state_vectors->vecs[ii].vec.vel.y = sentinel->stVec[ii].velY;
      meta->state_vectors->vecs[ii].vec.vel.z = sentinel->stVec[ii].velZ;
    }

    // Propagate the state vectors to start, center, end
    int vector_count = 3;
    double data_int = date_difference(&imgStopDate, &imgStopTime, 
              &imgStartDate, &imgStartTime) / 2.0;
    while (fabs(data_int) > 10.0) {
      data_int /= 2;
      vector_count = vector_count*2-1;
    }
    propagate_state(meta, vector_count, data_int);
  }
  
  // Transform block
  if (strcmp_case(sentinel->productType, "GRD") == 0)
    meta->transform = 
      gcp2transform(sentinel->gcp, sentinel->gcp_count, "ground");
  else if (strcmp_case(sentinel->productType, "SLC") == 0)
    meta->transform = 
      gcp2transform(sentinel->gcp, sentinel->gcp_count, "slant");

  // Location block
  if (strcmp_case(sentinel->productType, "SLC") == 0 ||
      strcmp_case(sentinel->productType, "GRD") == 0 ||
      strcmp_case(sentinel->productType, "OCN") == 0) {
    meta->location = meta_location_init();
    for (ii=0; ii<sentinel->gcp_count; ii++) {
      if (sentinel->gcp[ii].line == 0 && sentinel->gcp[ii].pixel == 0) {
        meta->location->lat_start_near_range = sentinel->gcp[ii].lat;
        meta->location->lon_start_near_range = sentinel->gcp[ii].lon;
      }
      else if (sentinel->gcp[ii].line == 0 && 
        sentinel->gcp[ii].pixel == sentinel->numberOfSamples-1) {
        meta->location->lat_start_far_range = sentinel->gcp[ii].lat;
        meta->location->lon_start_far_range = sentinel->gcp[ii].lon;
      }
      else if (sentinel->gcp[ii].line == sentinel->numberOfLines-1 && 
        sentinel->gcp[ii].pixel == 0) {
        meta->location->lat_end_near_range = sentinel->gcp[ii].lat;
        meta->location->lon_end_near_range = sentinel->gcp[ii].lon;
      }
      else if (sentinel->gcp[ii].line == sentinel->numberOfLines-1 &&
        sentinel->gcp[ii].pixel == sentinel->numberOfSamples-1) {
        meta->location->lat_end_far_range = sentinel->gcp[ii].lat;
        meta->location->lon_end_far_range = sentinel->gcp[ii].lon;
      }
    }
  }

  if (meta->sar) {
    // Determine center lat/lon, earth radius and satellite height
    meta_get_latLon(meta, meta->general->sample_count/2, 
      meta->general->line_count/2, 0.0, &lat, &lon);
    meta->general->center_latitude = lat;
    if (lon > 180.0)
      meta->general->center_longitude = lon - 360.0;
    else
      meta->general->center_longitude = lon;
  
    lat = meta->general->center_latitude * D2R;
    re = meta->general->re_major;
    rp = meta->general->re_minor;
    meta->sar->earth_radius = 
      (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
  
    ii = meta->state_vectors->vector_count / 2;
    double posx = meta->state_vectors->vecs[ii].vec.pos.x;
    double posy = meta->state_vectors->vecs[ii].vec.pos.y;
    double posz = meta->state_vectors->vecs[ii].vec.pos.z;
    meta->sar->satellite_height = sqrt(posx*posx + posy*posy + posz*posz);
  }

  return meta;
}
