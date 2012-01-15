#include "terrasar.h"
#include "doppler.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "asf_nan.h"
#include "xml_util.h"
#include "dateUtil.h"
#include <ctype.h>
#include "asf_tiff.h"
#include "geotiff_support.h"

void import_terrasar(const char *inBaseName, radiometry_t radiometry,
		     const char *outBaseName, int ampOnly)
{
  FILE *fpIn, *fpOut;
  terrasar_meta *terrasar;
  meta_parameters *meta;
  char *inDataName=NULL, *inMetaName=NULL, *outDataName=NULL, *path=NULL;
  char polarization[10], bands[30], radioStr[50];
  unsigned char intValue[4];
  float *amp = NULL, *phase = NULL, re, im, fValue;
  double incid;
  short int shortReal, shortImaginary;
  int ii, kk, ll, attribute;
  int asfv, aslv, rsfv, rslv;
  long long file_size;

  if (!fileExists(inBaseName))
    inMetaName = appendExt(inBaseName, ".xml");
  else {
    inMetaName = (char *) MALLOC(sizeof(char)*(strlen(inBaseName)+2));
    sprintf(inMetaName, "%s", inBaseName);
  }
  outDataName = appendExt(outBaseName, ".img");

  terrasar = read_terrasar_meta(inMetaName);
  asfPrintStatus("   ImageDataType: %s, imageDataFormat: %s\n",
		 terrasar->imageDataType, terrasar->imageDataFormat);
  switch (radiometry)
    {
    case r_AMP:
      sprintf(radioStr, "amplitude");
      break;
    case r_POWER:
      sprintf(radioStr, "power");
      break;
    case r_SIGMA:
      sprintf(radioStr, "calibrated sigma (power scale)");
      break;
    case r_SIGMA_DB:
      sprintf(radioStr, "calibrated sigma (decibel)");
      break;
    case r_BETA:
      sprintf(radioStr, "calibrated beta (power scale)");
      break;
    case r_BETA_DB:
      sprintf(radioStr, "calibrated beta (decibel)");
      break;
    case r_GAMMA:
      sprintf(radioStr, "calibrated gamma (power scale)");
      break;
    case r_GAMMA_DB:
      sprintf(radioStr, "calibrated gamma (decibel)");
      break;
    }
  asfPrintStatus("   Radiometry: %s\n", radioStr);
  meta = terrasar2meta(terrasar);
  if (!ISNAN(terrasar->cal_factor))
    create_cal_params(inMetaName, meta, REPORT_LEVEL_NONE);
  int db_flag = FALSE;
  if (radiometry >= r_SIGMA && radiometry <= r_GAMMA)
    meta->general->no_data = 0.0001;
  if (radiometry >= r_SIGMA_DB && radiometry <= r_GAMMA_DB) {
    meta->general->no_data = -40.0;
    db_flag = TRUE;
  }
  if (radiometry == r_SIGMA || radiometry == r_SIGMA_DB)
    meta->general->image_data_type = SIGMA_IMAGE;
  else if (radiometry == r_BETA || radiometry == r_BETA_DB)
    meta->general->image_data_type = BETA_IMAGE;
  else if (radiometry == r_GAMMA || radiometry == r_GAMMA_DB)
    meta->general->image_data_type = GAMMA_IMAGE;
  meta_write(meta, outDataName);

  if (!fileExists(inMetaName))
    asfPrintError("Metadata file (%s) does not exist!\n", inMetaName);
  xmlDoc *doc = xmlReadFile(inMetaName, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", inMetaName);

  int numberOfLayers = terrasar->numberOfLayers;
  if (ampOnly) {
    strcpy(meta->general->bands, "AMP");
    meta->general->band_count = 1;
    numberOfLayers = 1;
  }

  for (ii=0; ii<numberOfLayers; ii++) {
    attribute = xml_get_int_attribute(doc, 
      "level1Product.productComponents.imageData[%d].layerIndex", ii);
    if (attribute != (ii+1)) {
      printf("attribute: %i\n", attribute);
      asfPrintError("LayerIndex of imageData in metadata out of order!\n");
    }

    // path from the xml (metadata) file
    path = get_dirname(inBaseName);
    inDataName = (char *) MALLOC(sizeof(char)*(strlen(path)+100));
    if (strlen(path)>0) {
      strcpy(inDataName, path);
      if (inDataName[strlen(inDataName)-1] != '/')
        strcat(inDataName, "/");
    }
    else
      strcpy(inDataName, "");

    // strcat() on the path & file from the XML entry
    strcat(inDataName, xml_get_string_value(doc, 
      "level1Product.productComponents.imageData[%d].file.location.path", ii));
    strcat(inDataName, "/");
    strcat(inDataName, xml_get_string_value(doc, 
      "level1Product.productComponents.imageData[%d].file.location.filename", 
      ii));

    // checking that the size is plausible
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
      if (!ampOnly) {
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
    
      fpIn = FOPEN(inDataName, "rb");
      if (ii == 0)
	fpOut = FOPEN(outDataName, "wb");
      
      FREAD(&intValue, 1, 4, fpIn);
      int bytes_in_burst = bigInt32(intValue);
      FREAD(&intValue, 1, 4, fpIn);
      int range_sample_relative_index = bigInt32(intValue);;
      FREAD(&intValue, 1, 4, fpIn);
      int range_samples = bigInt32(intValue);
      FREAD(&intValue, 1, 4, fpIn);
      int azimuth_samples = bigInt32(intValue);
      FREAD(&intValue, 1, 4, fpIn);
      int burst_index = bigInt32(intValue);
      FREAD(&intValue, 1, 4, fpIn);
      int rangeline_total_number_bytes = bigInt32(intValue);
      FREAD(&intValue, 1, 4, fpIn);
      int total_number_lines = bigInt32(intValue);
      FREAD(&intValue, 1, 4, fpIn);
      
      // Check for the first and last azimuth and range pixel
      asfv = 1; // azimuth sample first valid
      aslv = azimuth_samples; // azimuth sample last valid
      rsfv = 1; // range sample first valid
      rslv = range_samples; // range sample last valid
      
      // Check valid range samples
      for (kk=4; kk<total_number_lines; kk++) {
	FSEEK(fpIn, 4*rangeline_total_number_bytes, SEEK_SET);
	FREAD(&intValue, 1, 4, fpIn);
	if (bigInt32(intValue) > rsfv)
	  rsfv = bigInt32(intValue);
	FREAD(&intValue, 1, 4, fpIn);
	if (bigInt32(intValue) < rslv)
	  rslv = bigInt32(intValue);
      }
      
      // Check first valid azimuth sample
      FSEEK(fpIn, 2*rangeline_total_number_bytes+8, SEEK_SET);
      for (kk=2; kk<rslv; kk++) {
	FREAD(&intValue, 1, 4, fpIn);
	if (bigInt32(intValue) > asfv)
	  asfv = bigInt32(intValue);
      }
      
      // Check last valid azimuth sample
      FSEEK(fpIn, 3*rangeline_total_number_bytes+8, SEEK_SET);
      for (kk=2; kk<rslv; kk++) {
	FREAD(&intValue, 1, 4, fpIn);
	if (bigInt32(intValue) < aslv)
	  rslv = kk - 1;
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
	put_band_float_line(fpOut, meta, ii*2, ll-3, amp);
	if (!ampOnly)
	  put_band_float_line(fpOut, meta, ii*2+1, ll-3, phase);
	asfLineMeter(ll, azimuth_samples);
      }
      meta_write(meta, outDataName);
      FREE(path);
      FREE(inDataName);
      FCLOSE(fpIn);
      FCLOSE(fpOut);
      FREE(amp);
      FREE(phase);
    }    
    else if (strcmp_case(terrasar->imageDataType, "DETECTED") == 0) {
      asfPrintStatus("\nIngesting data file %d/%d (%s) ...\n\n",
		     ii+1, terrasar->numberOfLayers, inDataName);
      strcpy(polarization, xml_get_string_value(doc, 
	"level1Product.productComponents.imageData[%d].polLayer", ii));
      if (ii == 0)
	sprintf(meta->general->bands, "%s", polarization);
      else {
	sprintf(bands, ",%s", polarization);
	strcat(meta->general->bands, bands);
	meta->general->band_count += 1;
      }
      
      TIFF *tiff = NULL;
      GTIF *gtif = NULL;
      data_type_t data_type;
      short sample_format, bits_per_sample, planar_config;
      short num_bands;
      int is_scanline_format, is_palette_color_tiff, wrong=FALSE;
      char str[512];
      char *error_message = (char *) MALLOC(sizeof(char)*2048);
      tiff = XTIFFOpen(inDataName, "r");
      if (!tiff)
	asfPrintError("Could not open data file (%s)\n", inDataName);
      gtif = GTIFNew(tiff);
      if (!gtif)
	asfPrintError("Could not read GeoTIFF keys from data file (%s)\n",
		      inDataName);
      uint32 tif_sample_count;
      uint32 tif_line_count;
      
      TIFFGetField(tiff, TIFFTAG_IMAGELENGTH, &tif_line_count);
      TIFFGetField(tiff, TIFFTAG_IMAGEWIDTH, &tif_sample_count);
      if ((meta->general->sample_count != tif_sample_count) ||
	  (meta->general->line_count != tif_line_count))
	asfPrintError(error_message, 
		      "Problem with image dimensions. Was looking for %d lines "
		      "and %d samples.\nFound %ld lines and %ld samples instead"
		      "!\n", 
		      meta->general->line_count, meta->general->sample_count, 
		      tif_line_count, tif_sample_count);
      
      // Check general TIFF tags
      get_tiff_data_config(tiff, &sample_format, &bits_per_sample, 
			   &planar_config, &data_type, &num_bands, 
			   &is_scanline_format, &is_palette_color_tiff, 
			   REPORT_LEVEL_WARNING);
      
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
	sprintf(str, "Problem with bits per sample. Was looking for 16, "
		"found %d instead!\n", bits_per_sample);
	strcat(error_message, str);
	wrong = TRUE;
      }
      if (data_type != INTEGER16) {
	strcat(error_message, "Problem with data type. Was looking INTEGER16,");
	if (data_type == BYTE)
	  strcat(error_message, " found BYTE instead!\n");
	else if (data_type == INTEGER32)
	  strcat(error_message, " found INTEGER32 instead!\n");
	else if (data_type == REAL32)
	  strcat(error_message, " found REAL32 instead!\n");
	else if (data_type == REAL64)
	  strcat(error_message, " found REAL64 instead!\n");
	else if (data_type == COMPLEX_BYTE)
	  strcat(error_message, " found COMPLEX_BYTE instead!\n");
	else if (data_type == COMPLEX_INTEGER16)
	  strcat(error_message, " found COMPLEX_INTEGER16 instead!\n");
	else if (data_type == COMPLEX_INTEGER32)
	  strcat(error_message, " found COMPLEX_INTEGER32 instead!\n");
	else if (data_type == COMPLEX_REAL32)
	  strcat(error_message, " found COMPLEX_REAL32 instead!\n");
	else if (data_type == COMPLEX_REAL64)
	  strcat(error_message, " found COMPLEX_REAL64 instead!\n");
	wrong = TRUE;
      }
      if (wrong)
	asfPrintError(error_message);
      
      tiff_type_t tiffInfo;
      get_tiff_type(tiff, &tiffInfo);
      if (tiffInfo.format != SCANLINE_TIFF &&
	  tiffInfo.format != STRIP_TIFF    &&
	  tiffInfo.format != TILED_TIFF)
	asfPrintError("Can't read the GeoTIFF file (%s). Unrecognized TIFF "
		      "type!\n", inDataName);
      
      uint32 scanlineSize = TIFFScanlineSize(tiff);
      tdata_t *tiff_buf = _TIFFmalloc(scanlineSize);
      if (!tiff_buf)
	asfPrintError("Can't allocate buffer for reading TIFF lines!\n");
      
      amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
      if (ii == 0)
	fpOut = FOPEN(outDataName, "wb");
      
      // Read file line by line
      uint32 row, sample;
      for (row=0; row<(uint32)meta->general->line_count; row++) {
	asfLineMeter(row, meta->general->line_count);
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
	for (sample=0; sample<(uint32)meta->general->sample_count; sample++) {
	  switch (sample_format)
	    {
	    case SAMPLEFORMAT_UINT:
	      fValue = (float)(((uint16*)tiff_buf)[sample]);
	      break;
	    case SAMPLEFORMAT_INT:
	      fValue = (float)(((int16*)tiff_buf)[sample]);
	      break;
	    }
	  if (radiometry >= r_SIGMA && radiometry <= r_GAMMA_DB) {
	    incid = meta_incid(meta, (int)row, (int)sample);
	    amp[sample] = get_cal_dn(meta, incid, sample, fValue, "", db_flag);
	  }
	  else
	    amp[sample] = fValue;
	}
	put_band_float_line(fpOut, meta, ii, (int)row, amp);
      }
      
      FREE(amp);
      _TIFFfree(tiff_buf);
      GTIFFree(gtif);
      XTIFFClose(tiff);
      meta_write(meta, outDataName);
      FREE(path);
      FREE(inDataName);
    }
    else
      asfPrintError("Data type (%s) and data format (%s) currently not "
		    "supported!\n", 
		    terrasar->imageDataType, terrasar->imageDataFormat);
  
  }
  FREE(terrasar);
  meta_free(meta);
  FREE(inMetaName);
  xmlFreeDoc(doc);
  xmlCleanupParser();
}

