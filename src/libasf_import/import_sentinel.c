#include "sentinel.h"
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

static float *get_cal_lut_lines(sentinel_lut_line *cal, radiometry_t radiometry,
  int band, int lut_line_count, int start, int end, char *outFile)
{
  meta_parameters *meta = meta_read(outFile);
  int sample_count = meta->general->sample_count;
  int oldLine = cal[start].line;
  int newLine = cal[end].line;
  int deltaLine = newLine - oldLine;
  if (oldLine == newLine)
    newLine++;
  int ii, kk, kkLut, oldPixel, newPixel, deltaPixel, idx;
  float cal00, cal01, cal10, cal11;
  float a00, a10, a01, a11, slopeLine, slopePixel;
  float *amp = (float *) MALLOC(sizeof(float)*sample_count*deltaLine);
  
  // Determine actual calibration value
  for (ii=oldLine; ii<newLine; ii++) {
    kkLut = 0;
    oldPixel = cal[start].pixel[0];
    newPixel = cal[start].pixel[1];
    deltaPixel = newPixel - oldPixel;
    cal00 = cal[start].value[0];
    cal10 = cal[start].value[1];
    cal01 = cal[end].value[0];
    cal11 = cal[end].value[1];
    a00 = cal00;
    a10 = cal10 - cal00;
    a01 = cal01 - cal00;
    a11 = cal00 - cal01 - cal10 + cal11;
    for (kk=0; kk<sample_count; kk++) {
      idx = (ii - oldLine)*sample_count + kk;
      if (kk == newPixel) {
        kkLut++;
        oldPixel = newPixel;
        if (kkLut+1 < cal[start].count)
          newPixel = cal[start].pixel[kkLut+1];
        else
          newPixel = cal[start].pixel[kkLut];
        deltaPixel = newPixel - oldPixel;
        cal00 = cal[start].value[kkLut];
        if (kkLut+1 < cal[start].count)
          cal10 = cal[start].value[kkLut+1];
        else
          cal10 = cal[start].value[kkLut];
        cal01 = cal[end].value[kkLut];
        if (kkLut+1 < cal[end].count)
          cal11 = cal[end].value[kkLut+1];
        else
          cal11 = cal[end].value[kkLut];
        a00 = cal00;
        a10 = cal10 - cal00;
        a01 = cal01 - cal00;
        a11 = cal00 - cal01 - cal10 + cal11;
      }
      slopeLine = (float)(ii - oldLine)/(float)deltaLine;
      slopePixel = (float)(kk - oldPixel)/(float)deltaPixel;
      idx = (ii - oldLine)*meta->general->sample_count + kk;
      amp[idx] = a00 + a10 * slopePixel + a01 * slopeLine 
        + a11 * slopePixel * slopeLine;
    }
  }
  return amp;
}

static void write_cal_lut(sentinel_lut_line *cal, radiometry_t radiometry, 
  int band, int lut_line_count, char *outFile)
{
  FILE *fp = NULL;

  asfPrintStatus("\n   Writing calibration LUT image (%s) to disk (%s) ...\n", 
    radiometry2str(radiometry), outFile);
  
  meta_parameters *meta = meta_read(outFile);
  float *amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  
  if (band == 0)
    fp = FOPEN(outFile, "wb");
  else
    fp = FOPEN(outFile, "ab");
  int oldLine = cal[0].line;
  int newLine = cal[1].line;
  int deltaLine = newLine - oldLine;
  int iiLut = 0, kkLut = 0, maxLine = 1;
  int ii, kk, oldPixel, newPixel, deltaPixel;
  float cal00, cal01, cal10, cal11;
  float a00, a10, a01, a11, slopeLine, slopePixel;
  for (ii=0; ii<meta->general->line_count; ii++) {
    if (ii == newLine) {
      oldLine = newLine;
      iiLut++;
      newLine = cal[iiLut+1].line;
      deltaLine = newLine - oldLine;
    }
    if (iiLut+1 <lut_line_count)
      maxLine = iiLut + 1;
    else
      maxLine = iiLut;
    kkLut = 0;
    oldPixel = cal[iiLut].pixel[0];
    newPixel = cal[iiLut].pixel[1];
    deltaPixel = newPixel - oldPixel;
    cal00 = cal[iiLut].value[0];
    cal10 = cal[iiLut].value[1];
    cal01 = cal[maxLine].value[0];
    cal11 = cal[maxLine].value[1];
    a00 = cal00;
    a10 = cal10 - cal00;
    a01 = cal01 - cal00;
    a11 = cal00 - cal01 - cal10 + cal11;
    for (kk=0; kk<meta->general->sample_count; kk++) {
      if (kk == newPixel) {
        kkLut++;
        oldPixel = newPixel;
        if (kkLut+1 < cal[iiLut].count)
          newPixel = cal[iiLut].pixel[kkLut+1];
        else
          newPixel = cal[iiLut].pixel[kkLut];
        deltaPixel = newPixel - oldPixel;
        cal00 = cal[iiLut].value[kkLut];
        if (kkLut+1 < cal[iiLut].count)
          cal10 = cal[iiLut].value[kkLut+1];
        else
          cal10 = cal[iiLut].value[kkLut];
        cal01 = cal[maxLine].value[kkLut];
        if (kkLut+1 < cal[maxLine].count)
          cal11 = cal[maxLine].value[kkLut+1];
        else
          cal11 = cal[maxLine].value[kkLut];
        a00 = cal00;
        a10 = cal10 - cal00;
        a01 = cal01 - cal00;
        a11 = cal00 - cal01 - cal10 + cal11;
      }
      slopeLine = (float)(ii - oldLine)/(float)deltaLine;
      slopePixel = (float)(kk - oldPixel)/(float)deltaPixel;
      amp[kk] = a00 + a10 * slopePixel + a01 * slopeLine 
        + a11 * slopePixel * slopeLine;
    }
    put_band_float_line(fp, meta, band, ii, amp);
    asfLineMeter(ii, meta->general->line_count);
  }
  FCLOSE(fp);
  meta_free(meta);
}

static float *get_noise_lut_lines(sentinel_lut_line *lut, radiometry_t radiometry, 
  int band, int lut_line_count, int start, int end, char *outFile)
{
  meta_parameters *meta = meta_read(outFile);
  int sample_count = meta->general->sample_count;
  int oldLine = lut[start].line;
  int newLine = lut[end].line;
  int deltaLine = newLine - oldLine;
  if (oldLine == newLine)
    newLine++;
  int ii, kk, kkLut, oldPixel, newPixel, deltaPixel, idx;
  float noise00, noise01, noise10, noise11;
  float a00, a10, a01, a11, slopeLine, slopePixel;
  float *noise = (float *) MALLOC(sizeof(float)*sample_count*deltaLine);
  
  // Determine actual value
  for (ii=oldLine; ii<newLine; ii++) {
    kkLut = 0;
    oldPixel = lut[start].pixel[0];
    newPixel = lut[start].pixel[1];
    deltaPixel = newPixel - oldPixel;
    noise00 = lut[start].value[0];
    noise10 = lut[start].value[1];
    noise01 = lut[end].value[0];
    noise11 = lut[end].value[1];
    a00 = noise00;
    a10 = noise10 - noise00;
    a01 = noise01 - noise00;
    a11 = noise00 - noise01 - noise10 + noise11;
    for (kk=0; kk<meta->general->sample_count; kk++) {
      idx = (ii - oldLine)*sample_count + kk;
      if (kk == newPixel) {
        kkLut++;
        oldPixel = newPixel;
        if (kkLut+1 < lut[start].count)
          newPixel = lut[start].pixel[kkLut+1];
        else
          newPixel = lut[start].pixel[kkLut];
        deltaPixel = newPixel - oldPixel;
        noise00 = lut[start].value[kkLut];
        if (kkLut+1 < lut[start].count)        
          noise10 = lut[start].value[kkLut+1];
        else
          noise10 = lut[start].value[kkLut];
        noise01 = lut[end].value[kkLut];
        if (kkLut+1 < lut[end].count)
          noise11 = lut[end].value[kkLut+1];
        else
          noise11 = lut[end].value[kkLut];
        a00 = noise00;
        a10 = noise10 - noise00;
        a01 = noise01 - noise00;
        a11 = noise00 - noise01 - noise10 + noise11;
      }
      slopeLine = (float)(ii - oldLine)/(float)deltaLine;
      slopePixel = (float)(kk - oldPixel)/(float)deltaPixel;
      noise[idx] = a00 + a10 * slopePixel + a01 * slopeLine 
        + a11 * slopePixel * slopeLine;
    }
  }
  return noise;
}

static void write_noise_lut(sentinel_lut_line *lut, radiometry_t radiometry, 
  int band, int lut_line_count, char *outFile)
{
  FILE *fp = NULL;

  asfPrintStatus("\n   Writing noise LUT image to disk (%s) ...\n", outFile);
  
  meta_parameters *meta = meta_read(outFile);
  float *noise = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  
  fp = FOPEN(outFile, "ab");
  int oldLine = lut[0].line;
  int newLine = lut[1].line;
  int deltaLine = newLine - oldLine;
  int iiLut = 0, kkLut = 0, maxLine = 1;
  int ii, kk, oldPixel, newPixel, deltaPixel;
  float noise00, noise01, noise10, noise11;
  float a00, a10, a01, a11, slopeLine, slopePixel;
  for (ii=0; ii<meta->general->line_count; ii++) {
    if (ii == newLine) {
      oldLine = newLine;
      iiLut++;
      if (iiLut+1 < lut_line_count)
        maxLine = iiLut + 1;
      else
        maxLine = iiLut;
      newLine = lut[maxLine].line;
      deltaLine = newLine - oldLine;
    }
    kkLut = 0;
    oldPixel = lut[iiLut].pixel[0];
    newPixel = lut[iiLut].pixel[1];
    deltaPixel = newPixel - oldPixel;
    noise00 = lut[iiLut].value[0];
    noise10 = lut[iiLut].value[1];
    noise01 = lut[maxLine].value[0];
    noise11 = lut[maxLine].value[1];
    a00 = noise00;
    a10 = noise10 - noise00;
    a01 = noise01 - noise00;
    a11 = noise00 - noise01 - noise10 + noise11;
    for (kk=0; kk<meta->general->sample_count; kk++) {
      if (kk == newPixel) {
        kkLut++;
        oldPixel = newPixel;
        if (kkLut+1 < lut[iiLut].count)
          newPixel = lut[iiLut].pixel[kkLut+1];
        else
          newPixel = lut[iiLut].pixel[kkLut];
        deltaPixel = newPixel - oldPixel;
        noise00 = lut[iiLut].value[kkLut];
        if (kkLut+1 < lut[iiLut].count)        
          noise10 = lut[iiLut].value[kkLut+1];
        else
          noise10 = lut[iiLut].value[kkLut];
        noise01 = lut[maxLine].value[kkLut];
        if (kkLut+1 < lut[maxLine].count)
          noise11 = lut[maxLine].value[kkLut+1];
        else
          noise11 = lut[maxLine].value[kkLut];
        a00 = noise00;
        a10 = noise10 - noise00;
        a01 = noise01 - noise00;
        a11 = noise00 - noise01 - noise10 + noise11;
      }
      slopeLine = (float)(ii - oldLine)/(float)deltaLine;
      slopePixel = (float)(kk - oldPixel)/(float)deltaPixel;
      noise[kk] = a00 + a10 * slopePixel + a01 * slopeLine 
        + a11 * slopePixel * slopeLine;
    }
    put_band_float_line(fp, meta, band, ii, noise);
    asfLineMeter(ii, meta->general->line_count);
  }
  FCLOSE(fp);
  meta_free(meta);
}

static sentinel_lut_line *read_sentinel_calibration(char *xmlFile, 
  radiometry_t radiometry, int *lutLines)
{
  int kk, ll, line, pixel_count;
  char *str = (char *) MALLOC(sizeof(char)*512);
  char pixelStr[8192], valueStr[16384], **pixel, **value;
  float minValue = 9999999;
  float maxValue = -9999999;

  // Initialization
  xmlDoc *doc = xmlReadFile(xmlFile, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", xmlFile); 
  xmlNode *root = xmlDocGetRootElement(doc);
  if (!root)
    asfPrintError("Could not get root element %s\n", xmlFile);
    
  // Get dimensions
  int line_count = xml_xpath_get_int_value(doc, 
    "/calibration/calibrationVectorList/@count");
  sentinel_lut_line *lut = 
    (sentinel_lut_line *) MALLOC(sizeof(sentinel_lut_line)*line_count);
    
  // Work your way through sigma, beta, gamma and dn
  for (kk=0; kk<line_count; kk++) {
    sprintf(str, 
      "/calibration/calibrationVectorList/calibrationVector[%d]/line", kk+1);
    line = xml_xpath_get_int_value(doc, str);
    sprintf(str, 
      "/calibration/calibrationVectorList/calibrationVector[%d]/pixel", kk+1);
    strcpy(pixelStr, xml_xpath_get_string_value(doc, str));
    split_into_array(pixelStr, ' ', &pixel_count, &pixel);
    lut[kk].line = line;
    lut[kk].count = pixel_count;
    lut[kk].pixel = (int *) MALLOC(sizeof(int)*pixel_count);
    lut[kk].value = (float *) MALLOC(sizeof(float)*pixel_count);

    if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB) {
      sprintf(str, 
        "/calibration/calibrationVectorList/calibrationVector[%d]/sigmaNought", 
        kk+1);
      strcpy(valueStr, xml_xpath_get_string_value(doc, str));
      split_into_array(valueStr, ' ', &pixel_count, &value);
      for (ll=0; ll<pixel_count; ll++) {
        lut[kk].line = line;
        lut[kk].pixel[ll] = atoi(pixel[ll]);
        lut[kk].value[ll] = atof(value[ll]);
        if (lut[kk].value[ll] < minValue)
          minValue = lut[kk].value[ll];
        if (lut[kk].value[ll] > maxValue)
          maxValue = lut[kk].value[ll];
      }
    }
    else if (radiometry == r_BETA || radiometry == r_BETA_DB) {
      sprintf(str, 
        "/calibration/calibrationVectorList/calibrationVector[%d]/betaNought", 
        kk+1);
      strcpy(valueStr, xml_xpath_get_string_value(doc, str));
      split_into_array(valueStr, ' ', &pixel_count, &value);
      for (ll=0; ll<pixel_count; ll++) {
        lut[kk].line = line;
        lut[kk].pixel[ll] = atoi(pixel[ll]);
        lut[kk].value[ll] = atof(value[ll]);
        if (lut[kk].value[ll] < minValue)
          minValue = lut[kk].value[ll];
        if (lut[kk].value[ll] > maxValue)
          maxValue = lut[kk].value[ll];
      }
    }
    else if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB) {
      sprintf(str, 
        "/calibration/calibrationVectorList/calibrationVector[%d]/gamma", kk+1);
      strcpy(valueStr, xml_xpath_get_string_value(doc, str));
      split_into_array(valueStr, ' ', &pixel_count, &value);
      for (ll=0; ll<pixel_count; ll++) {
        lut[kk].line = line;
        lut[kk].pixel[ll] = atoi(pixel[ll]);
        lut[kk].value[ll] = atof(value[ll]);
        if (lut[kk].value[ll] < minValue)
          minValue = lut[kk].value[ll];
        if (lut[kk].value[ll] > maxValue)
          maxValue = lut[kk].value[ll];
      }
    }
    else if (radiometry == r_AMP) {
      sprintf(str, 
        "/calibration/calibrationVectorList/calibrationVector[%d]/dn", kk+1);
      strcpy(valueStr, xml_xpath_get_string_value(doc, str));
      split_into_array(valueStr, ' ', &pixel_count, &value);
      for (ll=0; ll<pixel_count; ll++) {
        lut[kk].line = line;
        lut[kk].pixel[ll] = atoi(pixel[ll]);
        lut[kk].value[ll] = atof(value[ll]);
        if (lut[kk].value[ll] < minValue)
          minValue = lut[kk].value[ll];
        if (lut[kk].value[ll] > maxValue)
          maxValue = lut[kk].value[ll];
      }
    }
  }
  asfPrintStatus("   Calibration LUT values - minimum: %g, maximum: %g\n", 
    minValue, maxValue);
  xmlFreeDoc(doc);
  xmlCleanupParser();
  *lutLines = line_count;
  
  return lut;
}

static sentinel_lut_line *read_sentinel_noise(char *xmlFile, char *mode,
  int maxLine, int maxPixel, int *lutLines)
{
  int kk, ll, line, pixel_count;
  char *str = (char *) MALLOC(sizeof(char)*512);
  char *xpath = (char *) MALLOC(sizeof(char)*512);
  char *range = (char *) MALLOC(sizeof(char)*32);
  
  char pixelStr[8192], noiseStr[16384], **pixel, **noise;
  float minNoise = 9999999;
  float maxNoise = -9999999;

  // Initialization
  xmlDoc *doc = xmlReadFile(xmlFile, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", xmlFile); 
  xmlNode *root = xmlDocGetRootElement(doc);
  if (!root)
    asfPrintError("Could not get root element %s\n", xmlFile);
  
  // Get dimensions
  strcpy(xpath, "/noise/noiseVectorList");
  if (!xml_xpath_element_exists(doc, xpath)){
    asfPrintStatus("Using alternative noiseRangeVectorList");
    strcpy(range, "Range");
  }else{    
    strcpy(range, "");
  }
        
  sprintf(str, "/noise/noise%sVectorList/@count", range);
    
  int line_count = xml_xpath_get_int_value(doc, str);
  
  // Location of NoiseLut changed from noiseVectorList to noiseRangeVectorList
  sprintf(xpath, "/noise/noise%sVectorList/noise%sVector", range, range);  
  
  sentinel_lut_line *lut = 
    (sentinel_lut_line *) MALLOC(sizeof(sentinel_lut_line)*line_count);
    
  // Work your way through the noise LUT
  for (kk=0; kk<line_count; kk++) {
    sprintf(str, "%s[%d]/line", xpath, kk+1);
    line = xml_xpath_get_int_value(doc, str);
    sprintf(str, "%s[%d]/pixel", xpath, kk+1);
    strcpy(pixelStr, xml_xpath_get_string_value(doc, str));
    split_into_array(pixelStr, ' ', &pixel_count, &pixel);
    sprintf(str, "%s[%d]/noise%sLut", xpath, kk+1, range);
    strcpy(noiseStr, xml_xpath_get_string_value(doc, str));
    split_into_array(noiseStr, ' ', &pixel_count, &noise);
    lut[kk].line = line;
    lut[kk].count = pixel_count;
    lut[kk].pixel = (int *) MALLOC(sizeof(int)*pixel_count);
    lut[kk].value = (float *) MALLOC(sizeof(float)*pixel_count);
    for (ll=0; ll<pixel_count; ll++) {
      lut[kk].pixel[ll] = atoi(pixel[ll]);
      lut[kk].value[ll] = atof(noise[ll]);
      if (lut[kk].value[ll] < minNoise)
        minNoise = lut[kk].value[ll];
      if (lut[kk].value[ll] > maxNoise)
        maxNoise = lut[kk].value[ll];
    }
    lut[kk].pixel[pixel_count-1] = maxPixel;
  }
  lut[line_count-1].line = maxLine;
  asfPrintStatus("   Noise LUT values - minimum: %g, maximum: %g\n", 
    minNoise, maxNoise);
  xmlFreeDoc(doc);
  xmlCleanupParser();
  
  // Check whether noise floor is looking good
  if (maxNoise > 0 && maxNoise < 5e-09) {
    asfPrintStatus("   Noise floor is not within the expected value range!\n"
      "   Scaling noise floor values ...\n");
    minNoise = 9999999;
    maxNoise = -9999999;
    for (kk=0; kk<line_count; kk++)
      for (ll=0; ll<pixel_count; ll++) {
        lut[kk].value[ll] *= 1e12;
        if (lut[kk].value[ll] < minNoise)
          minNoise = lut[kk].value[ll];
        if (lut[kk].value[ll] > maxNoise)
          maxNoise = lut[kk].value[ll];
      }
    asfPrintStatus("   Noise LUT values - minimum: %g, maximum: %g\n", 
      minNoise, maxNoise);
    *lutLines = line_count;
  }
  else if (maxNoise > 0 && maxNoise < 5e-07) {
    asfPrintStatus("   Noise floor is not within the expected value range!\n"
      "   Scaling noise floor values ...\n");
    minNoise = 9999999;
    maxNoise = -9999999;
    for (kk=0; kk<line_count; kk++)
      for (ll=0; ll<pixel_count; ll++) {
        lut[kk].value[ll] *= 2e10;
        if (lut[kk].value[ll] < minNoise)
          minNoise = lut[kk].value[ll];
        if (lut[kk].value[ll] > maxNoise)
          maxNoise = lut[kk].value[ll];
      }
    asfPrintStatus("   Noise LUT values - minimum: %g, maximum: %g\n", 
      minNoise, maxNoise);
    *lutLines = line_count;
  }
  else if (maxNoise < 10.0) {
    char newXmlFile[1024];
    sprintf(newXmlFile, "%s%csentinel%cnoise-s1a-%s.xml", get_asf_share_dir(), 
      DIR_SEPARATOR, DIR_SEPARATOR, mode);
    asfPrintStatus("   Noise floor is not within the expected value range!\n"
      "   Replacing it with standard noise floor for beam mode (%s) ...\n",
      newXmlFile);
    for (kk=0; kk<line_count; kk++) {
      FREE(lut[kk].pixel);
      FREE(lut[kk].value);
    }
    FREE(lut);
    lut = (sentinel_lut_line *) MALLOC(sizeof(sentinel_lut_line)*line_count);
    lut = read_sentinel_noise(newXmlFile, mode, maxLine, maxPixel, lutLines);
  }
  else
    *lutLines = line_count;
  
  return lut;
}

void import_sentinel(const char *inBaseName, radiometry_t radiometry,
  const char *lutFile, const char *outBaseName)
{
  sentinel_files *files = NULL;
  meta_parameters *meta = NULL;
  char inDataName[1024], *outDataName=NULL;
  char mission[25], beamMode[10], productType[10];
  char mode[25], modeStr[25];
  float *amp = NULL, *phase = NULL, *calValue = NULL, *lutNoise = NULL;
  float noise, re, im, scaledPower;
  double noise_mean = 0.0;
  long pixelCount = 0; 
  float mask = MAGIC_UNSET_DOUBLE;
  int ii, file_count, band, line, sample, detected=TRUE, band_count=0;

  check_sentinel_meta(inBaseName, mission, beamMode, productType);
  asfPrintStatus("   Mission: %s, beam mode: %s, product type: %s\n",
    mission, beamMode, productType);
  if (strcmp_case(productType, "RAW") == 0)
    asfPrintError("Product type 'RAW' currently not supported!\n");  
  else if (strcmp_case(productType, "SLC") == 0 ||
    strcmp_case(productType, "GRD") == 0) {

    // Work out how many files we need to take care of
    if (strcmp_case(productType, "GRD") == 0)
      file_count = 1;
    else { // must be SLC data
      if (strcmp_case(beamMode, "IW") == 0)
        file_count = 3;
      else if (strcmp_case(beamMode, "EW") == 0)
        file_count = 5;
      else // must be strip mode
        file_count = 1;
    }

    files = (sentinel_files *) MALLOC(sizeof(sentinel_files)*2);

    // Go through the files and import them
    for (ii=0; ii<file_count; ii++) {
    
      if (file_count == 1)
        outDataName = appendExt(outBaseName, ".img");
      else {
        outDataName = (char *) MALLOC(sizeof(char)*1024);
        sprintf(outDataName, "%s%d.img", outBaseName, ii+1);
      }
      sentinel_meta *sentinel = read_sentinel_meta(inBaseName, ii+1);
      if (beamMode[0] == 'S')
        sprintf(mode, "SM-%s%c", productType, sentinel->resolution);
      else
        sprintf(mode, "%s-%s%c", beamMode, productType, sentinel->resolution);
      strcpy(modeStr, lc(mode));
      meta = sentinel2meta(sentinel);
      meta->general->radiometry = radiometry;
      meta_write(meta, outDataName);
      int sample_count = meta->general->sample_count;

      // Let's check the GeoTIFF data.
      TIFF *tiff = NULL;
      GTIF *gtif = NULL;
      data_type_t data_type;
      short sample_format, bits_per_sample, planar_config;
      short num_bands;
      int is_scanline_format, is_palette_color_tiff;
  
      FILE *fpOut = FOPEN(outDataName, "wb");
      band_count = meta->general->band_count;
      if (strcmp_case(sentinel->productType, "SLC") == 0)
        band_count /= 2;
      int noiseCount = 0;
      for (band=0; band<band_count; band++) {
  
        strcpy(inDataName, sentinel->data[band]);
        strcpy(files[band].polarization, sentinel->file[band].polarization);
        strcpy(files[band].annotation, sentinel->file[band].annotation);
        strcpy(files[band].calibration, sentinel->file[band].calibration);
        strcpy(files[band].noise, sentinel->file[band].noise);

        // Read calibration LUTs
        asfPrintStatus("\n   Reading calibration LUT (%s) ...\n",
          files[band].calibration);
        int calLutLines;
        sentinel_lut_line *cal = 
          read_sentinel_calibration(files[band].calibration, radiometry, 
            &calLutLines);
        
        // Read noise LUT
        asfPrintStatus("\n   Reading noise LUT (%s) ...\n", files[band].noise);
        int noiseLutLines;
        sentinel_lut_line *lut = 
          read_sentinel_noise(files[band].noise, modeStr, 
            meta->general->line_count, meta->general->sample_count, 
            &noiseLutLines);

        // Open GeoTIFF file
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
          asfPrintError( "Problem with image dimensions. Was looking for %d lines "
                "and %d samples.\nFound %ld lines and %ld samples instead!\n", 
                meta->general->line_count, meta->general->sample_count, 
                tif_line_count, tif_sample_count);
  
        // Check general TIFF tags
        get_tiff_data_config(tiff, &sample_format, &bits_per_sample, 
          &planar_config, &data_type, &num_bands, &is_scanline_format, 
          &is_palette_color_tiff, REPORT_LEVEL_WARNING);
  
        if (sample_format == SAMPLEFORMAT_UINT && bits_per_sample == 16)
          asfPrintStatus("   Found Unsigned 16 bit data\n");
        else if (sample_format == SAMPLEFORMAT_COMPLEXINT && 
          bits_per_sample == 32) {
          asfPrintStatus("   Found Complex 16 bit data\n");
          detected = FALSE;
        }
  
        // Check the image description
        const char *imageDescription;
        TIFFGetField(tiff, TIFFTAG_IMAGEDESCRIPTION, &imageDescription);
        if (strncmp_case(imageDescription, "SENTINEL-1", 10) != 0)
          asfPrintError("GeoTIFF file (%s) does not contain Sentinel data\n",
            inDataName);
      
        tiff_type_t tiffInfo;
        get_tiff_type(tiff, &tiffInfo);
        if (tiffInfo.format != SCANLINE_TIFF &&
        tiffInfo.format != STRIP_TIFF    &&
        tiffInfo.format != TILED_TIFF)
          asfPrintError("Can't read the GeoTIFF file (%s). Unrecognized TIFF "
                "type!\n", sentinel->data[band]);
  
        asfPrintStatus("\n   Importing %s ...\n", sentinel->data[band]);
  
        uint32 scanlineSize = TIFFScanlineSize(tiff);
        tdata_t *tiff_buf = _TIFFmalloc(scanlineSize);
        if (!tiff_buf)
          asfPrintError("Can't allocate buffer for reading TIFF lines!\n");
  
        amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
        phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  
        // Loop through the LUTs
        uint32 row;
        uint16 *intValue;
        int kkCal = 0, kkNoise = 0;
        int calIndex, calStart, calEnd, calLimit = 0;
        int noiseIndex, noiseStart, noiseEnd, noiseLimit = 0;
        for (line=0; line<meta->general->line_count; line++) {
        
          // Check LUT information
          if (line == calLimit) {
            if (calValue)
              FREE(calValue);
            if (kkCal < calLutLines - 1) {
              calStart = kkCal;
              calEnd = kkCal + 1;
            }
            else {
              calStart = calEnd = kkCal;
            }
            calLimit = cal[calEnd].line;
            calValue = get_cal_lut_lines(cal, radiometry, band, calLutLines, 
              calStart, calEnd, outDataName);
            kkCal++; 
          }
          if (line == noiseLimit) {
            if (lutNoise)
              FREE(lutNoise);
            if (kkNoise < noiseLutLines - 1) {
              noiseStart = kkNoise;
              noiseEnd = kkNoise + 1;
            }
            else {
              noiseStart = noiseEnd = kkNoise;
            }
            noiseLimit = lut[noiseEnd].line;
            lutNoise = get_noise_lut_lines(lut, radiometry, band, noiseLutLines, 
              noiseStart, noiseEnd, outDataName);
            kkNoise++;
          }

          // Apply values while going through data
          row = (uint32)line;
          switch (tiffInfo.format) 
          {
            case SCANLINE_TIFF:
              TIFFReadScanline(tiff, tiff_buf, row, 0);
              break;
            case STRIP_TIFF:
              ReadScanline_from_TIFF_Strip(tiff, tiff_buf, row, 0);
              break;
            case TILED_TIFF:
              ReadScanline_from_TIFF_TileRow(tiff, tiff_buf, row, 0);
              break;
            default:
              asfPrintError("Can't read this TIFF format!\n");
              break;
          }
          for (sample=0; sample<sample_count; sample++) {
            calIndex = (line - cal[calStart].line)*sample_count + sample;
            noiseIndex = (line - lut[noiseStart].line)*sample_count + sample;
            switch (sample_format)
            {
              case SAMPLEFORMAT_UINT:
                re = (float)(((uint16*)tiff_buf)[sample]);
                break;
              case SAMPLEFORMAT_COMPLEXINT:
                intValue = &((uint16*)tiff_buf)[sample];
                re = (float) intValue[0];
                im = (float) intValue[1];
                break;
            }
            if (detected) {
              noise = fabs(lutNoise[noiseIndex])/
                (calValue[calIndex]*calValue[calIndex]);
              if (noiseCount == 0) {
                if (ISNAN(mask) || !FLOAT_EQUIVALENT(noise, mask)) {
                  noise_mean += noise;
                  pixelCount++;
                }
              }
              scaledPower = (re*re - lutNoise[noiseIndex])/
                (calValue[calIndex]*calValue[calIndex]);
              if (radiometry == r_SIGMA_DB || radiometry == r_BETA_DB || 
                radiometry == r_GAMMA_DB)
                if (scaledPower < 0)
                  amp[sample] = -40.0;
                else
                  amp[sample] = 10.0 * log10(scaledPower);
              else
                amp[sample] = scaledPower;
            }
            else {
              amp[sample] = sqrt(re*re + im*im);
              phase[sample] = atan2(im, re);
            }
          }
          if (detected)
            put_band_float_line(fpOut, meta, band, line, amp);
          else {
            put_band_float_line(fpOut, meta, band*2, line, amp);
            put_band_float_line(fpOut, meta, band*2+1, line, phase);
          }
          asfLineMeter(line, meta->general->line_count);
        }
          
        noiseCount++;
        FREE(amp);
        FREE(phase);
        FREE(calValue);
        calValue = NULL;
        FREE(lutNoise);
        lutNoise = NULL;
        _TIFFfree(tiff_buf);
        GTIFFree(gtif);
        XTIFFClose(tiff);
      }
      FREE(sentinel->stVec);
      FREE(sentinel->gcp);
      for (band=0; band<255; band++)
        FREE(sentinel->data[band]);
      FREE(sentinel->data);
      FREE(sentinel->file);
      FREE(sentinel);
    }
    
    // Store the mean value for the noise floor
    meta->calibration = meta_calibration_init();
    meta->calibration->type = sentinel_cal;
    meta->calibration->sentinel = 
      (sentinel_cal_params *) MALLOC(sizeof(sentinel_cal_params));
    meta->calibration->sentinel->noise_mean = noise_mean/pixelCount;
    meta_write(meta, outDataName);
    meta_free(meta);
    FREE(files);
  }
  else if (strcmp_case(productType, "OCN") == 0) {
    if (strcmp_case(beamMode, "IW") == 0 ||
      strcmp_case(beamMode, "EW") == 0) {
      char **arr, xmlFile[512], granule[512], absPath[1024];
      int n;

      sentinel_meta *sentinel = read_sentinel_meta(inBaseName, 1);
      split_dir_and_file(sentinel->data[0], absPath, granule);
      split_into_array(sentinel->data[0], '/', &n, &arr);
      snprintf(xmlFile, strlen(arr[n-3])-4, "%s", arr[n-3]);
      strcat(xmlFile, ".xml");
      import_netcdf_xml(sentinel->data[0], xmlFile);
      outDataName = appendExt(outBaseName, ".img");
      meta_parameters *meta = sentinel2meta(sentinel);
      meta_write(meta, outDataName);
      meta_free(meta);
      FREE(sentinel->stVec);
      FREE(sentinel->gcp);
      for (band=0; band<255; band++)
        FREE(sentinel->data[band]);
      FREE(sentinel->data);
      FREE(sentinel);
    }
    else if (strcmp_case(beamMode, "WV") == 0)
      asfPrintError("Sentinel OCN WV data currently not supported!\n");
      outDataName = (char *) MALLOC(sizeof(char)*1024);
      for (ii=0; ii<file_count; ii++) {
        sprintf(outDataName, "%s%d.img", outBaseName, ii+1);
      }
  }
  FREE(outDataName);
}

