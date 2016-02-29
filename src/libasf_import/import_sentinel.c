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

static void write_cal_lut(sentinel_lut *cal, radiometry_t radiometry, int band,
  int lut_line_count, int lut_pixel_count, char *outFile)
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
  int newLine = cal[lut_pixel_count+1].line;
  int oldPixel = cal[0].pixel;
  int newPixel = cal[1].pixel;
  int iiLut = 0, kkLut = 0, minX, maxX, minY, maxY;
  int ii, kk, deltaLine, deltaPixel, lutLine, lutPixel;
  float a00, a10, a01, a11, slopeLine, slopePixel;
  for (ii=0; ii<meta->general->line_count; ii++) {
    if (ii == newLine) {
      oldLine = newLine;
      iiLut++;
      newLine = cal[iiLut*lut_pixel_count+kkLut].line;
    }
    kkLut = 0;
    newPixel = cal[iiLut*lut_pixel_count].pixel;
    for (kk=0; kk<meta->general->sample_count; kk++) {
      if (kk == newPixel) {
        oldPixel = newPixel;
        kkLut++;
        newPixel = cal[iiLut*lut_pixel_count+kkLut].pixel;
      }
      minX = kkLut;
      maxX = (kkLut == lut_pixel_count-1 ? kkLut : kkLut+1);
      minY = iiLut;
      maxY = (iiLut == lut_line_count-1 ? iiLut : iiLut+1);
      lutLine = cal[minY*lut_pixel_count+minX].line;
      deltaLine = cal[maxY*lut_pixel_count+minY].line - lutLine - 1;
      lutPixel = cal[minY*lut_pixel_count+minX].pixel;
      deltaPixel = cal[minY*lut_pixel_count+maxX].pixel - lutPixel - 1;
      if (radiometry == r_AMP) {
        a00 = cal[minY*lut_pixel_count+minX].dn;
        a10 = cal[minY*lut_pixel_count+maxX].dn - 
          cal[minY*lut_pixel_count+minX].dn;
        a01 = cal[maxY*lut_pixel_count+minX].dn - 
          cal[minY*lut_pixel_count+minX].dn;
        a11 = cal[minY*lut_pixel_count+minX].dn - 
          cal[minY*lut_pixel_count+maxX].dn -
          cal[maxY*lut_pixel_count+minX].dn + 
          cal[maxY*lut_pixel_count+maxX].dn;
      }
      else if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB) {
        a00 = cal[minY*lut_pixel_count+minX].sigma;
        a10 = cal[minY*lut_pixel_count+maxX].sigma - 
          cal[minY*lut_pixel_count+minX].sigma;
        a01 = cal[maxY*lut_pixel_count+minX].sigma - 
          cal[minY*lut_pixel_count+minX].sigma;
        a11 = cal[minY*lut_pixel_count+minX].sigma - 
          cal[minY*lut_pixel_count+maxX].sigma -
          cal[maxY*lut_pixel_count+minX].sigma + 
          cal[maxY*lut_pixel_count+maxX].sigma;
      }
      else if (radiometry == r_BETA || radiometry == r_BETA_DB) {
        a00 = cal[minY*lut_pixel_count+minX].beta;
        a10 = cal[minY*lut_pixel_count+maxX].beta - 
          cal[minY*lut_pixel_count+minX].beta;
        a01 = cal[maxY*lut_pixel_count+minX].beta - 
          cal[minY*lut_pixel_count+minX].beta;
        a11 = cal[minY*lut_pixel_count+minX].beta - 
          cal[minY*lut_pixel_count+maxX].beta -
          cal[maxY*lut_pixel_count+minX].beta + 
          cal[maxY*lut_pixel_count+maxX].beta;
      }
      else if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB) {
        a00 = cal[minY*lut_pixel_count+minX].beta;
        a10 = cal[minY*lut_pixel_count+maxX].beta - 
          cal[minY*lut_pixel_count+minX].beta;
        a01 = cal[maxY*lut_pixel_count+minX].beta - 
          cal[minY*lut_pixel_count+minX].beta;
        a11 = cal[minY*lut_pixel_count+minX].beta - 
          cal[minY*lut_pixel_count+maxX].beta -
          cal[maxY*lut_pixel_count+minX].beta + 
          cal[maxY*lut_pixel_count+maxX].beta;
      }
      if (deltaLine > 0)
        slopeLine = (float)(ii - lutLine)/(float)deltaLine;
      else
        slopeLine = 0.0;
      if (deltaPixel > 0)
        slopePixel = (float)(kk - lutPixel)/(float)deltaPixel;
      else
        slopePixel = 0.0;
      amp[kk] = a00 + a10 * slopePixel + a01 * slopeLine 
        + a11 * slopePixel * slopeLine;
    }
    put_band_float_line(fp, meta, band, ii, amp);
    asfLineMeter(ii, meta->general->line_count);
  }
  FCLOSE(fp);
  meta_free(meta);
}

static void write_noise_lut(sentinel_lut *lut, radiometry_t radiometry, 
  int band, int lut_line_count, int lut_pixel_count, char *outFile)
{
  FILE *fp = NULL;

  asfPrintStatus("\n   Writing noise LUT image to disk (%s) ...\n", outFile);
  
  meta_parameters *meta = meta_read(outFile);
  float *noise = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  
  fp = FOPEN(outFile, "ab");
  int oldLine = lut[0].line;
  int newLine = lut[lut_pixel_count+1].line;
  int oldPixel = lut[0].pixel;
  int newPixel = lut[1].pixel;
  int iiLut = 0, kkLut = 0, minX, maxX, minY, maxY;
  int ii, kk, deltaLine, deltaPixel, lutLine, lutPixel;
  float a00, a10, a01, a11, slopeLine, slopePixel;
  for (ii=0; ii<meta->general->line_count; ii++) {
    if (ii == newLine) {
      oldLine = newLine;
      iiLut++;
      newLine = lut[iiLut*lut_pixel_count+kkLut].line;
    }
    kkLut = 0;
    newPixel = lut[iiLut*lut_pixel_count].pixel;
    for (kk=0; kk<meta->general->sample_count; kk++) {
      if (kk == newPixel) {
        oldPixel = newPixel;
        kkLut++;
        newPixel = lut[iiLut*lut_pixel_count+kkLut].pixel;
      }
      minX = kkLut;
      maxX = (kkLut == lut_pixel_count-1 ? kkLut : kkLut+1);
      minY = iiLut;
      maxY = (iiLut == lut_line_count-1 ? iiLut : iiLut+1);
      lutLine = lut[minY*lut_pixel_count+minX].line;
      deltaLine = lut[maxY*lut_pixel_count+minY].line - lutLine - 1;
      lutPixel = lut[minY*lut_pixel_count+minX].pixel;
      deltaPixel = lut[minY*lut_pixel_count+maxX].pixel - lutPixel - 1;
      a00 = lut[minY*lut_pixel_count+minX].noise;
      a10 = lut[minY*lut_pixel_count+maxX].noise - 
        lut[minY*lut_pixel_count+minX].noise;
      a01 = lut[maxY*lut_pixel_count+minX].noise - 
        lut[minY*lut_pixel_count+minX].noise;
      a11 = lut[minY*lut_pixel_count+minX].noise - 
        lut[minY*lut_pixel_count+maxX].noise -
        lut[maxY*lut_pixel_count+minX].noise + 
        lut[maxY*lut_pixel_count+maxX].noise;
      if (deltaLine > 0)
        slopeLine = (float)(ii - lutLine)/(float)deltaLine;
      else
        slopeLine = 0.0;
      if (deltaPixel > 0)
        slopePixel = (float)(kk - lutPixel)/(float)deltaPixel;
      else
        slopePixel = 0.0;
      noise[kk] = a00 + a10 * slopePixel + a01 * slopeLine 
        + a11 * slopePixel * slopeLine;
    }
    put_band_float_line(fp, meta, band, ii, noise);
    asfLineMeter(ii, meta->general->line_count);
  }
  FCLOSE(fp);
  meta_free(meta);
}

static sentinel_lut *read_sentinel_calibration(char *xmlFile, int *lutLines, 
  int *lutPixels)
{
  int kk, ll, line;
  long idx;
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
  int pixel_count = xml_xpath_get_int_value(doc,
    "/calibration/calibrationVectorList/calibrationVector/pixel/@count");
  sentinel_lut *lut = 
    (sentinel_lut *) MALLOC(sizeof(sentinel_lut)*line_count*pixel_count);
    
  // Work your way through sigma, beta, gamma and dn
  for (kk=0; kk<line_count; kk++) {
    sprintf(str, 
      "/calibration/calibrationVectorList/calibrationVector[%d]/line", kk+1);
    line = xml_xpath_get_int_value(doc, str);
    sprintf(str, 
      "/calibration/calibrationVectorList/calibrationVector[%d]/pixel", kk+1);
    strcpy(pixelStr, xml_xpath_get_string_value(doc, str));
    split_into_array(pixelStr, ' ', &pixel_count, &pixel);

    // sigma
    sprintf(str, 
      "/calibration/calibrationVectorList/calibrationVector[%d]/sigmaNought", 
      kk+1);
    strcpy(valueStr, xml_xpath_get_string_value(doc, str));
    split_into_array(valueStr, ' ', &pixel_count, &value);
    for (ll=0; ll<pixel_count; ll++) {
      idx = kk*pixel_count + ll;
      lut[idx].line = line;
      lut[idx].pixel = atoi(pixel[ll]);
      lut[idx].sigma = atof(value[ll]);
      if (lut[idx].sigma < minValue)
        minValue = lut[idx].sigma;
      if (lut[idx].sigma > maxValue)
        maxValue = lut[idx].sigma;
    }
    
    // beta
    sprintf(str, 
      "/calibration/calibrationVectorList/calibrationVector[%d]/betaNought", 
      kk+1);
    strcpy(valueStr, xml_xpath_get_string_value(doc, str));
    split_into_array(valueStr, ' ', &pixel_count, &value);
    for (ll=0; ll<pixel_count; ll++) {
      idx = kk*pixel_count + ll;
      lut[idx].beta = atof(value[ll]);
      if (lut[idx].beta < minValue)
        minValue = lut[idx].beta;
      if (lut[idx].beta > maxValue)
        maxValue = lut[idx].beta;
    }
    
    // gamma
    sprintf(str, 
      "/calibration/calibrationVectorList/calibrationVector[%d]/gamma", kk+1);
    strcpy(valueStr, xml_xpath_get_string_value(doc, str));
    split_into_array(valueStr, ' ', &pixel_count, &value);
    for (ll=0; ll<pixel_count; ll++) {
      idx = kk*pixel_count + ll;
      lut[idx].gamma = atof(value[ll]);
      if (lut[idx].gamma < minValue)
        minValue = lut[idx].gamma;
      if (lut[idx].gamma > maxValue)
        maxValue = lut[idx].gamma;
    }
    
    // dn
    sprintf(str, 
      "/calibration/calibrationVectorList/calibrationVector[%d]/dn", kk+1);
    strcpy(valueStr, xml_xpath_get_string_value(doc, str));
    split_into_array(valueStr, ' ', &pixel_count, &value);
    for (ll=0; ll<pixel_count; ll++) {
      idx = kk*pixel_count + ll;
      lut[idx].dn = atof(value[ll]);
      if (lut[idx].dn < minValue)
        minValue = lut[idx].dn;
      if (lut[idx].dn > maxValue)
        maxValue = lut[idx].dn;
    }
  }
  asfPrintStatus("   Calibration LUT values - minimum: %g, maximum: %g\n", 
    minValue, maxValue);
  xmlFreeDoc(doc);
  xmlCleanupParser();
  *lutLines = line_count;
  *lutPixels = pixel_count;
  
  return lut;
}

static sentinel_lut *read_sentinel_noise(char *xmlFile, int *lutLines, 
  int *lutPixels)
{
  int kk, ll, line;
  long idx;
  char *str = (char *) MALLOC(sizeof(char)*512);
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
  int line_count = xml_xpath_get_int_value(doc, 
    "/noise/noiseVectorList/@count");
  int pixel_count = xml_xpath_get_int_value(doc,
    "/noise/noiseVectorList/noiseVector/pixel/@count");
  sentinel_lut *lut = 
    (sentinel_lut *) MALLOC(sizeof(sentinel_lut)*line_count*pixel_count);
    
  // Work your way through the noise LUT
  for (kk=0; kk<line_count; kk++) {
    sprintf(str, "/noise/noiseVectorList/noiseVector[%d]/line", kk+1);
    line = xml_xpath_get_int_value(doc, str);
    sprintf(str, "/noise/noiseVectorList/noiseVector[%d]/pixel", kk+1);
    strcpy(pixelStr, xml_xpath_get_string_value(doc, str));
    split_into_array(pixelStr, ' ', &pixel_count, &pixel);
    sprintf(str, "/noise/noiseVectorList/noiseVector[%d]/noiseLut", kk+1);
    strcpy(noiseStr, xml_xpath_get_string_value(doc, str));
    split_into_array(noiseStr, ' ', &pixel_count, &noise);
    for (ll=0; ll<pixel_count; ll++) {
      idx = kk*pixel_count + ll;
      lut[idx].line = line;
      lut[idx].pixel = atoi(pixel[ll]);
      lut[idx].noise = atof(noise[ll]);
      if (lut[idx].noise < minNoise)
        minNoise = lut[idx].noise;
      if (lut[idx].noise > maxNoise)
        maxNoise = lut[idx].noise;
    }
  }
  asfPrintStatus("   Noise LUT values - minimum: %g, maximum: %g\n", 
    minNoise, maxNoise);
  xmlFreeDoc(doc);
  xmlCleanupParser();
  *lutLines = line_count;
  *lutPixels = pixel_count;
  
  return lut;
}

void import_sentinel(const char *inBaseName, radiometry_t radiometry,
  const char *lutFile, const char *outBaseName)
{
  sentinel_files *files = NULL;
  meta_parameters *meta = NULL;
  meta_parameters *metaCal = NULL;
  meta_parameters *metaNoise = NULL;
  char inDataName[1024], *outDataName=NULL, outCalName[1024], outNoiseName[1024];
  char mission[25], beamMode[10], productType[10], bandName[25];
  float *amp = NULL, *phase = NULL, *calValue = NULL, *lutNoise = NULL;
  float *noise = NULL, *tmp = NULL, re, im;
  double noise_mean = 0.0;
  long pixelCount = 0; 
  float mask = MAGIC_UNSET_DOUBLE;
  int ii, file_count, band, sample, detected=TRUE, band_count=0;

  check_sentinel_meta(inBaseName, mission, beamMode, productType);
  asfPrintStatus("   Mission: %s, beam mode: %s, product type: %s\n",
    mission, beamMode, productType);
  if (strcmp_case(productType, "RAW") == 0)
    asfPrintError("Product type 'RAW' currently not supported!\n");  
  else if (strcmp_case(productType, "SLC") == 0 ||
    strcmp_case(productType, "GRD") == 0) {

    // Create temporary directory
    char tmpDir[1024];
    strcpy(tmpDir, "browse-");
    strcat(tmpDir, time_stamp_dir());
    create_clean_dir(tmpDir);
    asfPrintStatus("Temp dir is: %s\n", tmpDir);

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
      if (lutFile)
        strcpy(outCalName, lutFile);
      else
        sprintf(outCalName, "%s%c%s_lut.img", tmpDir, DIR_SEPARATOR,outBaseName);
      sprintf(outNoiseName, "%s%c%s_noise.img", 
        tmpDir, DIR_SEPARATOR, outBaseName);
      sentinel_meta *sentinel = read_sentinel_meta(inBaseName, ii+1);
      meta = sentinel2meta(sentinel);
      meta->general->radiometry = radiometry;
      meta_write(meta, outDataName);

      // Let's check the GeoTIFF data.
      TIFF *tiff = NULL;
      GTIF *gtif = NULL;
      data_type_t data_type;
      short sample_format, bits_per_sample, planar_config;
      short num_bands;
      int is_scanline_format, is_palette_color_tiff;
  
      FILE *fpOut = FOPEN(outDataName, "wb");
      FILE *fpNoise = FOPEN(outNoiseName, "wb");
      band_count = meta->general->band_count;
      if (strcmp_case(sentinel->productType, "SLC") == 0)
        band_count /= 2;
      int lutCount = 0;
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
        int lutLines, lutPixels;
        sentinel_lut *cal = read_sentinel_calibration(files[band].calibration, 
          &lutLines, &lutPixels);
        if (lutCount == 0) {
          metaCal = meta_read(outDataName);
          sprintf(metaCal->general->bands, "LUT_%s_%s", 
            radiometry2str(radiometry), files[band].polarization);
          metaNoise = meta_read(outDataName);
          sprintf(metaNoise->general->bands, "NOISE_%s", 
            files[band].polarization);
        }
        else {
          metaCal = meta_read(outCalName);
          sprintf(bandName, ",LUT_%s_%s", radiometry2str(radiometry),
            files[band].polarization);
          strcat(metaCal->general->bands, bandName);
          metaNoise = meta_read(outNoiseName);
          sprintf(bandName, ",NOISE_%s", files[band].polarization);
          strcat(metaNoise->general->bands, bandName);
        }
        metaCal->general->band_count = lutCount + 1;
        meta_write(metaCal, outCalName);
        metaNoise->general->band_count = noiseCount + 1;
        meta_write(metaNoise, outNoiseName);
        write_cal_lut(cal, radiometry, lutCount, lutLines, lutPixels, 
          outCalName);
        lutCount++;
        
        // Read noise LUT
        asfPrintStatus("\n   Reading noise LUT (%s) ...\n", files[band].noise);
        sentinel_lut *lut = read_sentinel_noise(files[band].noise, 
          &lutLines, &lutPixels);
        metaCal = meta_read(outCalName);
        sprintf(bandName, ",LUT_NOISE_%s", files[band].polarization);
        strcat(metaCal->general->bands, bandName);
        metaCal->general->band_count = lutCount + 1;
        meta_write(metaCal, outCalName);
        write_noise_lut(lut, radiometry, lutCount, lutLines, lutPixels, 
          outCalName);
        lutCount++;

        // Import GeoTIFF file
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
  
        // If we made it here, we are reasonably sure that we have the file that
        // we are looking for.
        asfPrintStatus("\n   Importing %s ...\n", sentinel->data[band]);
  
        uint32 scanlineSize = TIFFScanlineSize(tiff);
        tdata_t *tiff_buf = _TIFFmalloc(scanlineSize);
        if (!tiff_buf)
          asfPrintError("Can't allocate buffer for reading TIFF lines!\n");
  
        amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
        phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
        calValue = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
        lutNoise = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
        noise = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
        float scaledPower;
  
        // Read file line by line
        uint32 row;
        uint16 *intValue;
        FILE *fpCal = FOPEN(outCalName, "rb");
        for (row=0; row<(uint32)meta->general->line_count; row++) {
          asfLineMeter(row, meta->general->line_count);
          get_band_float_line(fpCal, metaCal, lutCount-2, (int)row, calValue);
          get_band_float_line(fpCal, metaCal, lutCount-1, (int)row, lutNoise);
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
          for (sample=0; sample<meta->general->sample_count; sample++) {
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
              noise[sample] = 
                fabs(lutNoise[sample])/(calValue[sample]*calValue[sample]);
              if (noiseCount == 0) {
                if (ISNAN(mask) || !FLOAT_EQUIVALENT(noise[sample], mask)) {
                  noise_mean += noise[sample];
                  pixelCount++;
                }
              }
              if (re*re > noise[sample])
                scaledPower = 
                  (re*re - noise[sample])/(calValue[sample]*calValue[sample]);
              else
                scaledPower = 0.0001;
              if (radiometry == r_SIGMA_DB || radiometry == r_BETA_DB || 
                radiometry == r_GAMMA_DB)
                amp[sample] = 10.0 * log10(scaledPower);
              else
                amp[sample] = scaledPower;
            }
            else {
              amp[sample] = sqrt(re*re + im*im);
              phase[sample] = atan2(im, re);
            }
          }
          if (detected) {
            put_band_float_line(fpOut, meta, band, (int)row, amp);
            put_band_float_line(fpNoise, metaNoise, noiseCount, (int)row, noise);
          }
          else {
            put_band_float_line(fpOut, meta, band*2, (int)row, amp);
            put_band_float_line(fpOut, meta, band*2+1, (int)row, phase);
          }
        }
        
        noiseCount++;
        FREE(amp);
        FREE(phase);
        FREE(calValue);
        FREE(lutNoise);
        FREE(noise);
        if (tmp)
          FREE(tmp);
        _TIFFfree(tiff_buf);
        GTIFFree(gtif);
        XTIFFClose(tiff);
        FCLOSE(fpCal);
        meta_free(metaCal);
        meta_free(metaNoise);
      }
      FCLOSE(fpOut);
      FCLOSE(fpNoise);
      FREE(sentinel->stVec);
      FREE(sentinel->gcp);
      for (band=0; band<255; band++)
        FREE(sentinel->data[band]);
      FREE(sentinel->data);
      FREE(sentinel->file);
      FREE(sentinel);

      // Clean up
      asfPrintStatus("Removing temporary directory: %s\n", tmpDir);
      remove_dir(tmpDir);
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
    FREE(tmp);
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

